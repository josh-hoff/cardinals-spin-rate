# =============================================================================
# cardinals_full_pipeline.R
# 
# Complete pipeline — run this file ONCE to:
#   1. Install required packages
#   2. Pull Cardinals Statcast data from Baseball Savant
#   3. Scrub and clean the data
#   4. Build the spin rate dashboard HTML
#   5. Schedule itself to run automatically every day at 8:00 AM
#
# INSTRUCTIONS:
#   - Put this file in your project folder (e.g. ~/cardinals/)
#   - Open R or RStudio and run: source("cardinals_full_pipeline.R")
#   - That's it. It will run once now, then every day at 8 AM automatically.
# =============================================================================


# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Automatically detects whether running on GitHub Actions or locally
if (nchar(Sys.getenv("GITHUB_ACTIONS")) > 0) {
  # Running on GitHub Actions — use the repo root
  PROJECT_DIR <- Sys.getenv("GITHUB_WORKSPACE")
} else {
  # Running locally — set this to your local cardinals folder
  PROJECT_DIR <- "C:/Users/joshh/OneDrive/Desktop/Coding Projects/cardinals"
}

DATA_DIR     <- file.path(PROJECT_DIR, "cardinals_data")
LATEST_CSV   <- file.path(DATA_DIR,    "cardinals_clean_LATEST.csv")
DASHBOARD    <- file.path(PROJECT_DIR, "cardinals_dashboard.html")
LOG_FILE     <- file.path(PROJECT_DIR, "pipeline.log")

# Cardinals team ID (STL = 138 in MLB API)
TEAM_ID <- 138
SEASON  <- 2026

# Fallback pitcher IDs in case roster API fails
FALLBACK_IDS <- c(
  669467, 669461, 700241, 669160, 681517, 677865, 694335, 592773,
  668941, 700669, 676617, 666277, 690928, 681676, 688297, 690155,
  694363, 691008, 802408
)


# ── STEP 1: INSTALL PACKAGES ──────────────────────────────────────────────────
log_msg <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    log_msg(paste("Installing package:", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}

install_if_missing("baseballr")
install_if_missing("dplyr")
install_if_missing("jsonlite")
install_if_missing("cronR")   # Mac/Linux scheduler
# Windows users: install taskscheduleR instead
# install_if_missing("taskscheduleR")

library(baseballr)
library(dplyr)
library(jsonlite)


# ── DYNAMIC ROSTER LOOKUP ─────────────────────────────────────────────────────
get_pitcher_ids <- function() {
  log_msg("Fetching current Cardinals pitching roster from MLB API...")
  active_ids <- tryCatch({
    roster <- mlb_rosters(team_id = TEAM_ID, season = SEASON, roster_type = "active")
    pitchers <- roster[roster$position_abbreviation %in% c("SP","RP","P"), ]
    ids <- as.integer(pitchers$person_id)
    log_msg(paste("Found", length(ids), "pitchers on active roster"))
    ids
  }, error = function(e) {
    log_msg(paste("Active roster lookup failed:", e$message))
    integer(0)
  })

  forty_man_ids <- tryCatch({
    roster <- mlb_rosters(team_id = TEAM_ID, season = SEASON, roster_type = "40Man")
    pitchers <- roster[roster$position_abbreviation %in% c("SP","RP","P"), ]
    as.integer(pitchers$person_id)
  }, error = function(e) {
    log_msg(paste("40-man roster lookup failed:", e$message))
    integer(0)
  })

  # Combine active + 40-man + fallback IDs (covers former players who appeared in 2026 data)
  all_ids <- unique(c(active_ids, forty_man_ids, FALLBACK_IDS))

  if (length(all_ids) == 0) {
    log_msg("All roster lookups failed, using fallback IDs only")
    return(FALLBACK_IDS)
  }

  log_msg(paste("Total pitcher IDs to pull (active + 40-man + fallback):", length(all_ids)))
  return(all_ids)
}

get_roster_status <- function() {
  log_msg("Fetching roster status for all pitcher tiers...")

  safe_get <- function(roster_type) {
    tryCatch({
      mlb_rosters(team_id = TEAM_ID, season = SEASON, roster_type = roster_type)
    }, error = function(e) {
      log_msg(paste("Could not fetch", roster_type, "roster:", e$message))
      NULL
    })
  }

  active_roster <- safe_get("active")
  full_roster   <- safe_get("40Man")

  # Active pitcher IDs
  active_ids <- integer(0)
  if (!is.null(active_roster)) {
    pitchers <- active_roster[active_roster$position_abbreviation %in% c("SP","RP","P"), ]
    active_ids <- as.integer(pitchers$person_id)
  }

  # IL pitcher IDs — D15/D60 = injured list, RM = restricted/minors (40-man, not IL)
  injured_ids <- integer(0)
  forty_man_ids <- integer(0)
  if (!is.null(full_roster)) {
    full_pitchers <- full_roster[full_roster$position_abbreviation %in% c("SP","RP","P"), ]
    if ("status_code" %in% names(full_pitchers)) {
      il_codes <- c("D15","D60","D10","D7")
      injured_ids   <- as.integer(full_pitchers$person_id[full_pitchers$status_code %in% il_codes])
      forty_man_ids <- as.integer(full_pitchers$person_id[
        !full_pitchers$person_id %in% c(active_ids, injured_ids)
      ])
    } else {
      forty_man_ids <- as.integer(setdiff(full_pitchers$person_id, active_ids))
    }
  }

  log_msg(paste("Roster tiers — Active:", length(active_ids),
                "| Injured:", length(injured_ids),
                "| 40-Man:", length(forty_man_ids)))

  list(
    active    = active_ids,
    injured   = injured_ids,
    forty_man = forty_man_ids
  )
}

pull_cardinals_data <- function() {
  log_msg("Starting data pull from Baseball Savant...")
  
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

  # Get current roster dynamically
  pitcher_ids <- get_pitcher_ids()
  
  all_pitches <- list()
  
  for (id in pitcher_ids) {
    log_msg(paste("  Pulling pitcher ID:", id))
    tryCatch({
      df <- statcast_search(
        start_date = paste0(SEASON, "-03-25"),
        end_date   = Sys.Date(),
        playerid   = id,
        player_type = "pitcher"
      )
      if (!is.null(df) && nrow(df) > 0) {
        all_pitches[[as.character(id)]] <- df
        log_msg(paste("    →", nrow(df), "pitches"))
      }
      Sys.sleep(3) # be polite to the Savant API
    }, error = function(e) {
      log_msg(paste("    ERROR:", e$message))
    })
  }
  
  if (length(all_pitches) == 0) stop("No data pulled — check your internet connection.")
  
  raw <- bind_rows(all_pitches)
  
  # Save timestamped raw file
  ts    <- format(Sys.time(), "%Y%m%d_%H%M%S")
  raw_path <- file.path(DATA_DIR, paste0("cardinals_raw_", ts, ".csv"))
  write.csv(raw, raw_path, row.names = FALSE)
  log_msg(paste("Raw data saved:", raw_path, "(", nrow(raw), "rows )"))
  
  raw
}


# ── STEP 3: SCRUB DATA ────────────────────────────────────────────────────────
scrub_cardinals_data <- function(raw, spin_min = 100, spin_max = 4000) {
  log_msg("Scrubbing data...")
  
  df <- raw
  
  # 1. Drop deprecated columns
  depr <- grep("deprecated", names(df), value = TRUE)
  df   <- df[, !names(df) %in% depr]
  log_msg(paste("  Dropped deprecated columns:", length(depr)))
  
  # 2. Deduplicate
  before <- nrow(df)
  df <- df[!duplicated(df[, c("game_pk","at_bat_number","pitch_number","pitcher","batter")]), ]
  log_msg(paste("  Deduplication removed:", before - nrow(df), "rows"))
  
  # 3. Remove automatic balls / intent walked
  before <- nrow(df)
  df <- df[!df$description %in% c("automatic_ball","intent_walked"), ]
  log_msg(paste("  Auto-balls removed:", before - nrow(df), "rows"))
  
  # 4. Remove pitch outs / intentional balls
  before <- nrow(df)
  df <- df[!df$pitch_name %in% c("Pitch Out","Intentional Ball"), ]
  log_msg(paste("  Pitch outs removed:", before - nrow(df), "rows"))
  
  # 5. Drop rows missing core fields
  before <- nrow(df)
  df <- df[!is.na(df$release_spin_rate) & !is.na(df$release_speed) & !is.na(df$pitch_name), ]
  log_msg(paste("  Missing core fields removed:", before - nrow(df), "rows"))
  
  # 6. Remove spin rate outliers
  before <- nrow(df)
  df <- df[df$release_spin_rate >= spin_min & df$release_spin_rate <= spin_max, ]
  log_msg(paste("  Spin outliers removed:", before - nrow(df), "rows"))
  
  # 7. Derive fields
  df$inning_numeric    <- round(df$inning + df$outs_when_up / 3, 4)
  df$release_spin_rate <- as.integer(round(df$release_spin_rate))
  df$release_speed     <- round(df$release_speed, 1)
  df$game_date         <- as.character(df$game_date)
  
  # 8. Validate pitchers
  found <- length(unique(df$player_name))
  log_msg(paste("  Pitchers in clean data:", found))
  
  # 9. Save clean CSV
  ts         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive    <- file.path(DATA_DIR, paste0("cardinals_clean_", ts, ".csv"))
  write.csv(df, LATEST_CSV,  row.names = FALSE)
  write.csv(df, archive,     row.names = FALSE)
  log_msg(paste("  Clean data saved:", nrow(df), "pitches →", LATEST_CSV))
  
  df
}


# ── STEP 4: BUILD DASHBOARD ───────────────────────────────────────────────────
build_dashboard <- function(df) {
  log_msg("Building dashboard...")
  
  cols <- c("player_name","pitch_name","inning_numeric","inning","outs_when_up",
            "release_spin_rate","release_speed","description","game_date",
            "game_pk","outing_progression","opponent","home_away","events","pitcher")
  cols <- cols[cols %in% names(df)]
  df <- df[, cols]
  
  n_pitches  <- nrow(df)
  n_pitchers <- length(unique(df$player_name))
  date_min   <- min(df$game_date)
  date_max   <- max(df$game_date)
  generated  <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  
  rows_json <- toJSON(
    lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, ]))),
    auto_unbox = TRUE
  )
  
  html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Cardinals Pitching — Spin Rate Dashboard</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:system-ui,sans-serif;background:#fcfcfb;color:#0b0b0b;padding:1.5rem 1.5rem 2rem}
h1{font-size:17px;font-weight:500;margin-bottom:4px}
.sub{font-size:12px;color:#898781;margin-bottom:1.25rem}
.controls{display:flex;gap:10px;flex-wrap:wrap;margin-bottom:1rem;align-items:flex-end}
.cg{display:flex;flex-direction:column;gap:4px}
.cg label{font-size:11px;font-weight:500;text-transform:uppercase;letter-spacing:.06em;color:#898781}
select{height:36px;padding:0 10px;border-radius:8px;border:1px solid #ccc;background:#fff;color:#0b0b0b;font-size:13px;cursor:pointer;min-width:175px}
.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:9px;margin-bottom:1rem}
.card{background:#f1efe8;border-radius:8px;padding:.65rem .9rem}
.card .lbl{font-size:11px;color:#898781;margin-bottom:2px;text-transform:uppercase;letter-spacing:.05em}
.card .val{font-size:20px;font-weight:500;color:#0b0b0b}
.card .unit{font-size:11px;color:#52514e;margin-top:1px}
.cw{position:relative;width:100%;height:420px;margin-bottom:.75rem}
.leg{display:flex;flex-wrap:wrap;gap:12px}
.li{display:flex;align-items:center;gap:5px;font-size:12px;color:#52514e}
.dot{width:10px;height:10px;border-radius:50%;flex-shrink:0}
#tt{position:fixed;background:#fff;border:1px solid #ccc;border-radius:8px;padding:8px 12px;
    font-size:12px;pointer-events:none;opacity:0;z-index:9999;transition:opacity .1s;
    box-shadow:0 2px 8px rgba(0,0,0,.12);max-width:200px}
#ttn{font-weight:500;margin-bottom:3px;color:#0b0b0b}
#ttb{color:#52514e;line-height:1.7}
</style>
</head>
<body>
<h1>Cardinals pitching &#8212; spin rate by inning</h1>
<div class="sub">', n_pitches, ' pitches &middot; ', date_min, ' &ndash; ', date_max,
' &middot; ', n_pitchers, ' pitchers &middot; last updated ', generated, '</div>
<div class="controls">
  <div class="cg"><label>Pitcher</label><select id="pSel"></select></div>
  <div class="cg"><label>Pitch type</label><select id="ptSel"></select></div>
  <div class="cg"><label>Result</label>
    <select id="rSel">
      <option value="all">All results</option>
      <option value="whiff">Whiffs</option>
      <option value="called_strike">Called strikes</option>
      <option value="in_play">In play</option>
      <option value="ball">Balls</option>
      <option value="foul">Fouls</option>
    </select>
  </div>
</div>
<div class="cards">
  <div class="card"><div class="lbl">Avg spin</div><div class="val" id="cAvg">&#8212;</div><div class="unit">RPM</div></div>
  <div class="card"><div class="lbl">Peak spin</div><div class="val" id="cMax">&#8212;</div><div class="unit">RPM</div></div>
  <div class="card"><div class="lbl">Pitches shown</div><div class="val" id="cCnt">&#8212;</div><div class="unit">of ', n_pitches, ' total</div></div>
  <div class="card"><div class="lbl">Avg velocity</div><div class="val" id="cVelo">&#8212;</div><div class="unit">MPH</div></div>
</div>
<div class="cw"><canvas id="sc"></canvas></div>
<div class="leg" id="leg"></div>
<div id="tt"><div id="ttn"></div><div id="ttb"></div></div>
<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"></script>
<script>
const COLORS={"4-Seam Fastball":"#2a78d6","Sinker":"#1baf7a","Slider":"#eda100",
  "Sweeper":"#eb6834","Changeup":"#e34948","Curveball":"#4a3aa7",
  "Cutter":"#e87ba4","Knuckle Curve":"#008300","Split-Finger":"#888780"};
const DISPLAY={
  "Pallante, Andre":"Andre Pallante","Liberatore, Matthew":"Matthew Liberatore",
  "McGreevy, Michael":"Michael McGreevy","May, Dustin":"Dustin May",
  "Leahy, Kyle":"Kyle Leahy","Bruihl, Justin":"Justin Bruihl",
  "Svanson, Matt":"Matt Svanson","Stanek, Ryne":"Ryne Stanek",
  "Romero, JoJo":"JoJo Romero","Graceffo, Gordon":"Gordon Graceffo",
  "O\'Brien, Riley":"Riley O\'Brien","Soriano, George":"George Soriano",
  "Dobbins, Hunter":"Hunter Dobbins","Fernandez, Ryan":"Ryan Fernandez",
  "Roycroft, Chris":"Chris Roycroft","Pushard, Matt":"Matt Pushard",
  "Shuster, Jared":"Jared Shuster","Rajcic, Max":"Max Rajcic","Mautz, Brycen":"Brycen Mautz"
};
const WHIFF=["swinging_strike","swinging_strike_blocked","foul_tip"];
const INPLAY=["hit_into_play","hit_by_pitch"];
const BALLS=["ball","blocked_ball","pitchout"];
const FOULS=["foul","foul_bunt","foul_pitchout"];
function rg(d){
  if(WHIFF.includes(d))return"whiff";
  if(d==="called_strike")return"called_strike";
  if(INPLAY.includes(d))return"in_play";
  if(BALLS.includes(d))return"ball";
  if(FOULS.includes(d))return"foul";
  return"other";
}
const ALL_ROWS=', rows_json, ';
const BY_PITCHER={};
ALL_ROWS.forEach(r=>{const p=r[0];if(!BY_PITCHER[p])BY_PITCHER[p]=[];BY_PITCHER[p].push(r);});
let chart=null;
function getPitchTypes(player){return Array.from(new Set((BY_PITCHER[player]||[]).map(r=>r[1]))).sort();}
function populatePitchTypes(player){
  const sel=document.getElementById("ptSel");
  sel.innerHTML="<option value=\'all\'>All pitches</option>";
  getPitchTypes(player).forEach(pt=>{const o=document.createElement("option");o.value=o.textContent=pt;sel.appendChild(o);});
}
function render(){
  const player=document.getElementById("pSel").value;
  const pitchType=document.getElementById("ptSel").value;
  const resultFilter=document.getElementById("rSel").value;
  const rows=BY_PITCHER[player]||[];
  const types=pitchType==="all"?getPitchTypes(player):[pitchType];
  const datasets=types.map(pt=>{
    const pts=rows.filter(r=>r[1]===pt&&(resultFilter==="all"||rg(r[7])===resultFilter));
    return{label:pt,data:pts.map(r=>({x:r[2],y:r[5],raw:r})),
      backgroundColor:(COLORS[pt]||"#888")+"bb",borderColor:COLORS[pt]||"#888",
      borderWidth:.5,pointRadius:3.5,pointHoverRadius:6};
  });
  const allPts=datasets.flatMap(d=>d.data);
  if(allPts.length){
    const spins=allPts.map(p=>p.y),velos=allPts.map(p=>p.raw[6]);
    document.getElementById("cAvg").textContent=Math.round(spins.reduce((a,b)=>a+b)/spins.length).toLocaleString();
    document.getElementById("cMax").textContent=Math.max(...spins).toLocaleString();
    document.getElementById("cCnt").textContent=allPts.length.toLocaleString();
    document.getElementById("cVelo").textContent=(velos.reduce((a,b)=>a+b)/velos.length).toFixed(1);
  } else {
    ["cAvg","cMax","cCnt","cVelo"].forEach(id=>document.getElementById(id).textContent="--");
  }
  const leg=document.getElementById("leg");leg.innerHTML="";
  types.forEach(pt=>{
    const cnt=rows.filter(r=>r[1]===pt).length;
    const div=document.createElement("div");div.className="li";
    const dot=document.createElement("div");dot.className="dot";dot.style.background=COLORS[pt]||"#888";
    const lbl=document.createElement("span");lbl.textContent=pt;
    const c=document.createElement("span");c.style.cssText="color:#898781;font-size:11px";
    c.textContent=" ("+cnt+")";div.append(dot,lbl,c);leg.appendChild(div);
  });
  if(chart)chart.destroy();
  const allX=allPts.map(p=>p.x);
  const xMax=allX.length?Math.max(...allX)+0.5:9;
  chart=new Chart(document.getElementById("sc").getContext("2d"),{
    type:"scatter",data:{datasets},
    options:{responsive:true,maintainAspectRatio:false,animation:{duration:150},
      layout:{padding:{top:10,right:14}},
      scales:{
        x:{type:"linear",min:.8,max:Math.max(xMax,9.2),
          title:{display:true,text:"Inning (with outs)",color:"#898781",font:{size:12}},
          ticks:{color:"#898781",font:{size:11},stepSize:1/3,autoSkip:false,maxRotation:0,
            callback(v){const i=Math.floor(v+.001),f=v-i;
              if(Math.abs(f)<.04)return String(i);
              if(Math.abs(f-.3333)<.04)return i+" \u2153";
              if(Math.abs(f-.6667)<.04)return i+" \u2154";
              return"";}},
          grid:{color:"rgba(0,0,0,.06)"}},
        y:{title:{display:true,text:"Spin rate (RPM)",color:"#898781",font:{size:12}},
          ticks:{color:"#898781",font:{size:11},callback:v=>v.toLocaleString()},
          grid:{color:"rgba(0,0,0,.06)"},min:600,max:3600}
      },
      plugins:{legend:{display:false},tooltip:{enabled:false}},
      onHover(e,els){
        const tt=document.getElementById("tt");
        if(!els.length){tt.style.opacity=0;return;}
        const r=datasets[els[0].datasetIndex].data[els[0].index].raw;
        document.getElementById("ttn").textContent=r[1];
        document.getElementById("ttb").innerHTML=
          "Spin: <b>"+r[5].toLocaleString()+" RPM</b><br>"+
          "Velo: "+r[6].toFixed(1)+" MPH<br>"+
          "Inning: "+r[3]+(r[4]>0?" ("+r[4]+" out"+(r[4]>1?"s":"")+")" :"")+
          "<br>Result: "+r[7].replace(/_/g," ")+"<br>"+r[8];
        tt.style.left=(e.native.clientX+14)+"px";tt.style.top=(e.native.clientY-20)+"px";tt.style.opacity=1;
      }
    }
  });
  document.getElementById("sc").addEventListener("mouseleave",()=>{document.getElementById("tt").style.opacity=0;});
}
const pitchers=Object.keys(BY_PITCHER).sort((a,b)=>(DISPLAY[a]||a).localeCompare(DISPLAY[b]||b));
const pSel=document.getElementById("pSel");
pitchers.forEach(p=>{const o=document.createElement("option");o.value=p;o.textContent=DISPLAY[p]||p;pSel.appendChild(o);});
pSel.addEventListener("change",()=>{populatePitchTypes(pSel.value);document.getElementById("ptSel").value="all";render();});
document.getElementById("ptSel").addEventListener("change",render);
document.getElementById("rSel").addEventListener("change",render);
populatePitchTypes(pitchers[0]);
render();
</script>
</body>
</html>')
  
  writeLines(html, DASHBOARD)
  log_msg(paste("Dashboard written:", DASHBOARD,
                sprintf("(%.0f KB)", file.size(DASHBOARD) / 1024)))
}


# ── STEP 5: FULL PIPELINE ─────────────────────────────────────────────────────
run_pipeline <- function() {
  log_msg("========== Pipeline started ==========")

  # ── Try API first ────────────────────────────────────────────────────────────
  api_success <- FALSE
  tryCatch({
    log_msg("Attempting API pull from Baseball Savant...")
    raw   <- pull_cardinals_data()
    clean <- scrub_cardinals_data(raw)
    api_success <- TRUE
    log_msg("API pull successful.")
  }, error = function(e) {
    log_msg(paste("API pull failed:", e$message))
  })

  # ── Fall back to CSV if API failed ───────────────────────────────────────────
  if (!api_success) {
    if (file.exists(LATEST_CSV)) {
      log_msg("Falling back to existing CSV: cardinals_clean_LATEST.csv")
    } else {
      log_msg("PIPELINE ERROR: API failed and no fallback CSV found. Aborting.")
      return(invisible(NULL))
    }
  }

  # ── Build dashboard ──────────────────────────────────────────────────────────
  tryCatch({
    # Fetch roster status for pitcher tier coloring
    roster_status <- get_roster_status()

    source(file.path(PROJECT_DIR, "build_dashboard.R"))
    build_cardinals_dashboard(
      clean_csv      = LATEST_CSV,
      output_html    = DASHBOARD,
      roster_status  = roster_status
    )
    log_msg("========== Pipeline complete ==========")
  }, error = function(e) {
    log_msg(paste("DASHBOARD ERROR:", e$message))
  })
}


# ── STEP 6: SCHEDULE DAILY AT 8:00 AM ────────────────────────────────────────
schedule_pipeline <- function() {

  # ── Mac / Linux (uses cronR) ───────────────────────────────────────────────
  if (.Platform$OS.type == "unix") {
    library(cronR)
    
    script_path <- normalizePath("cardinals_full_pipeline.R")
    rscript     <- Sys.which("Rscript")
    
    cmd <- cron_rscript(
      rscript    = script_path,
      rscript_bin = rscript,
      log_append = TRUE,
      log_file   = normalizePath(LOG_FILE, mustWork = FALSE)
    )
    
    cron_add(
      command  = cmd,
      frequency = "daily",
      at        = "08:00",
      id        = "cardinals_pipeline",
      description = "Cardinals daily Statcast pull + dashboard rebuild"
    )
    
    message("Cron job scheduled — pipeline will run every day at 8:00 AM.")
    message("To view scheduled jobs: cron_ls()")
    message("To remove:             cron_rm('cardinals_pipeline')")
    
  # ── Windows (uses taskscheduleR) ───────────────────────────────────────────
  } else {
    if (!requireNamespace("taskscheduleR", quietly = TRUE))
      install.packages("taskscheduleR", repos = "https://cloud.r-project.org")
    library(taskscheduleR)
    
    script_path <- normalizePath("cardinals_full_pipeline.R")
    
    taskscheduler_create(
      taskname  = "cardinals_pipeline",
      rscript   = script_path,
      schedule  = "DAILY",
      starttime = "08:00",
      startdate = format(Sys.Date(), "%m/%d/%Y")
    )
    
    message("Windows task scheduled — pipeline will run every day at 8:00 AM.")
    message("To view:   taskscheduler_ls()")
    message("To remove: taskscheduler_delete('cardinals_pipeline')")
  }
}


# ── RUN EVERYTHING ────────────────────────────────────────────────────────────
# Only auto-run if PROJECT_DIR exists and this is not being sourced interactively
if (exists("PROJECT_DIR") && nchar(PROJECT_DIR) > 0) {
  setwd(PROJECT_DIR)
  run_pipeline()
  # Only schedule on local machine, not on GitHub Actions
  if (nchar(Sys.getenv("GITHUB_ACTIONS")) == 0) {
    schedule_pipeline()
  }
}
