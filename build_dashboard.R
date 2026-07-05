# =============================================================================
# build_dashboard.R
#
# Reads cardinals_clean_LATEST.csv and writes a self-contained
# cardinals_dashboard.html with all data and Chart.js embedded.
#
# Usage:
#   setwd("C:/Users/joshh/OneDrive/Desktop/Coding Projects/cardinals")
#   source("build_dashboard.R")
#   build_cardinals_dashboard()
# =============================================================================

library(dplyr)
library(jsonlite)

build_cardinals_dashboard <- function(
    clean_csv     = "cardinals_data/cardinals_clean_LATEST.csv",
    output_html   = "cardinals_dashboard.html",
    roster_status = NULL
) {

  message("Reading: ", clean_csv)
  df <- read.csv(clean_csv, stringsAsFactors = FALSE)

  # ── Scrub ──────────────────────────────────────────────────────────────────
  depr <- grep("deprecated", names(df), value = TRUE)
  if (length(depr) > 0) df <- df[, !names(df) %in% depr]
  df <- df[!duplicated(df[, intersect(c("game_pk","at_bat_number","pitch_number","pitcher","batter"), names(df))]), ]
  df <- df[!df$description %in% c("automatic_ball","intent_walked"), ]
  df <- df[!df$pitch_name  %in% c("Pitch Out","Intentional Ball"), ]
  df <- df[!is.na(df$release_spin_rate) & !is.na(df$release_speed) & !is.na(df$pitch_name), ]
  df <- df[df$release_spin_rate >= 100 & df$release_spin_rate <= 4000, ]

  # Derive fields if not already present
  if (!"inning_numeric" %in% names(df))
    df$inning_numeric <- round(df$inning + df$outs_when_up / 3, 4)
  df$release_spin_rate <- as.integer(round(df$release_spin_rate))
  df$release_speed     <- round(df$release_speed, 1)
  df$game_date         <- as.character(df$game_date)
  df$events            <- ifelse(is.na(df$events), "", as.character(df$events))

  if (!"opponent" %in% names(df) && "home_team" %in% names(df)) {
    df$is_home   <- df$home_team == "STL"
    df$opponent  <- ifelse(df$is_home, df$away_team, df$home_team)
    df$home_away <- ifelse(df$is_home, "Home", "Away")
  }

  if (!"outing_progression" %in% names(df)) {
    df <- df %>%
      group_by(player_name, game_pk) %>%
      mutate(
        entry_inning    = min(inning),
        entry_outs_when = min(outs_when_up[inning == min(inning)])
      ) %>%
      ungroup() %>%
      mutate(
        total_outs         = (inning - 1) * 3 + outs_when_up,
        entry_total_outs   = (entry_inning - 1) * 3 + entry_outs_when,
        outs_into_outing   = total_outs - entry_total_outs,
        outing_progression = round(1 + outs_into_outing / 3, 4)
      )
  }

  # Keep dashboard columns
  cols <- c("player_name","pitch_name","inning_numeric","inning","outs_when_up",
            "release_spin_rate","release_speed","description","game_date",
            "game_pk","outing_progression","opponent","home_away","events","pitcher")
  cols <- cols[cols %in% names(df)]
  df   <- df[, cols]

  n_pitches  <- nrow(df)
  n_pitchers <- length(unique(df$player_name))
  message(sprintf("Scrubbed: %d pitches, %d pitchers", n_pitches, n_pitchers))

  # ── Serialize data ─────────────────────────────────────────────────────────
  rows_json <- as.character(toJSON(
    lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, ]))),
    auto_unbox = TRUE
  ))

  # ── Roster status JSON ─────────────────────────────────────────────────────
  if (!is.null(roster_status)) {
    roster_json <- as.character(toJSON(list(
      active    = as.integer(roster_status$active),
      injured   = as.integer(roster_status$injured),
      forty_man = as.integer(roster_status$forty_man)
    ), auto_unbox = FALSE))
  } else {
    roster_json <- '{"active":[],"injured":[],"forty_man":[]}'
  }

  # ── Download / load Chart.js ───────────────────────────────────────────────
  chartjs_cache <- "chart.umd.js"
  if (!file.exists(chartjs_cache)) {
    message("Downloading Chart.js (one-time)...")
    download.file(
      "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js",
      chartjs_cache, quiet = TRUE
    )
  }
  chartjs <- paste(readLines(chartjs_cache, encoding = "UTF-8", warn = FALSE), collapse = "\n")

  # ── Write HTML ─────────────────────────────────────────────────────────────
  out <- file(output_html, open = "wt", encoding = "UTF-8")

  # HEAD & CSS
  writeLines('<!DOCTYPE html>', out)
  writeLines('<html lang="en">', out)
  writeLines('<head>', out)
  writeLines('<meta charset="UTF-8">', out)
  writeLines('<meta name="viewport" content="width=device-width,initial-scale=1">', out)
  writeLines('<title>St. Louis Cardinals 2026 Pitching Staff Spin Rate Analyzer</title>', out)
  writeLines('<style>', out)
  writeLines('*{box-sizing:border-box;margin:0;padding:0}', out)
  writeLines('body{font-family:system-ui,sans-serif;background:#fcfcfb;color:#0b0b0b;padding:1.5rem 1.5rem 2rem}', out)
  writeLines('h1{font-size:17px;font-weight:500;margin-bottom:1rem}', out)
  writeLines('.controls{display:flex;gap:10px;flex-wrap:wrap;margin-bottom:1rem;align-items:flex-end}', out)
  writeLines('.cg{display:flex;flex-direction:column;gap:4px}', out)
  writeLines('.cg label{font-size:11px;font-weight:500;text-transform:uppercase;letter-spacing:.06em;color:#898781}', out)
  writeLines('.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:9px;margin-bottom:1rem}', out)
  writeLines('.card{background:#f1efe8;border-radius:8px;padding:.65rem .9rem}', out)
  writeLines('.card .lbl{font-size:11px;color:#898781;margin-bottom:2px;text-transform:uppercase;letter-spacing:.05em}', out)
  writeLines('.card .val{font-size:20px;font-weight:500;color:#0b0b0b}', out)
  writeLines('.card .unit{font-size:11px;color:#52514e;margin-top:1px}', out)
  writeLines('.cw{position:relative;overflow-x:auto;width:100%;height:min(60vh,600px);min-height:300px;margin-bottom:.5rem}', out)
  writeLines('.zoom-bar{display:none;align-items:center;gap:10px;margin-bottom:.75rem}', out)
  writeLines('.zoom-bar span{font-size:12px;color:#2a78d6;font-weight:500}', out)
  writeLines('.zoom-bar button{font-size:11px;padding:3px 10px;border-radius:6px;border:1px solid #2a78d6;background:#fff;color:#2a78d6;cursor:pointer}', out)
  writeLines('.leg{display:flex;flex-wrap:wrap;gap:12px;margin-top:.25rem}', out)
  writeLines('.li{display:flex;align-items:center;gap:5px;font-size:12px;color:#52514e}', out)
  writeLines('.dot{width:10px;height:10px;border-radius:50%;flex-shrink:0}', out)
  writeLines('#tt{position:fixed;background:#fff;border:1px solid #ccc;border-radius:8px;padding:8px 12px;font-size:12px;pointer-events:none;opacity:0;z-index:9999;transition:opacity .1s;box-shadow:0 2px 8px rgba(0,0,0,.12);max-width:210px}', out)
  writeLines('#ttn{font-weight:500;margin-bottom:3px;color:#0b0b0b}', out)
  writeLines('#ttb{color:#52514e;line-height:1.7}', out)
  writeLines('.cd-wrap{position:relative;min-width:200px}', out)
  writeLines('.cd-btn{height:36px;padding:0 10px;border-radius:8px;border:1px solid #ccc;background:#fff;color:#0b0b0b;font-size:13px;cursor:pointer;width:100%;text-align:left;display:flex;align-items:center;justify-content:space-between;user-select:none}', out)
  writeLines('.cd-arrow{font-size:10px;color:#898781;margin-left:8px;flex-shrink:0}', out)
  writeLines('.cd-list{display:none;position:absolute;top:40px;left:0;z-index:9998;background:#fff;border:1px solid #ccc;border-radius:8px;box-shadow:0 4px 12px rgba(0,0,0,.12);min-width:100%;max-height:220px;overflow-y:auto;padding:4px 0}', out)
  writeLines('.cd-list.open{display:block}', out)
  writeLines('.cd-item{display:flex;align-items:center;gap:8px;padding:5px 12px;font-size:13px;cursor:pointer;color:#0b0b0b;white-space:nowrap}', out)
  writeLines('.cd-item:hover{background:#f1efe8}', out)
  writeLines('.cd-item input[type=checkbox]{cursor:pointer;accent-color:#0b0b0b;flex-shrink:0}', out)
  writeLines('.cd-item label{cursor:pointer;flex:1}', out)
  writeLines('.cd-all{display:flex;align-items:center;justify-content:space-between;padding:5px 12px;font-size:13px;font-weight:500}', out)
  writeLines('.cd-all-left{display:flex;align-items:center;gap:8px}', out)
  writeLines('.cd-reset{font-size:11px;padding:2px 8px;border-radius:6px;border:1px solid #ccc;background:#f1efe8;cursor:pointer;color:#52514e;white-space:nowrap}', out)
  writeLines('.cd-divider{border:none;border-top:1px solid #eee;margin:3px 0}', out)
  writeLines('.toggle-wrap{display:flex;flex-direction:column;gap:4px}', out)
  writeLines('.toggle-wrap label{font-size:11px;font-weight:500;text-transform:uppercase;letter-spacing:.06em;color:#898781}', out)
  writeLines('.toggle{display:flex;border-radius:8px;border:1px solid #ccc;overflow:hidden;height:36px}', out)
  writeLines('.toggle button{flex:1;border:none;background:#fff;color:#52514e;font-size:12px;cursor:pointer;padding:0 14px;transition:background .15s,color .15s;white-space:nowrap}', out)
  writeLines('.toggle button.active{background:#0b0b0b;color:#fff}', out)
  writeLines('.clear-btn{height:36px;padding:0 16px;border-radius:8px;border:1px solid #e34948;background:#fff;color:#e34948;font-size:13px;cursor:pointer;font-weight:500;white-space:nowrap}', out)
  writeLines('select#pSel{height:36px;padding:0 10px;border-radius:8px;border:1px solid #ccc;background:#fff;font-size:13px;cursor:pointer;min-width:175px}', out)
  writeLines('</style>', out)
  writeLines('</head>', out)
  writeLines('<body>', out)

  # TITLE
  writeLines('<h1>St. Louis Cardinals 2026 Pitching Staff Spin Rate Analyzer</h1>', out)

  # CONTROLS
  writeLines('<div class="controls">', out)
  writeLines('  <div class="cg"><label>Pitcher</label>', out)
  writeLines('    <select id="pSel"></select>', out)
  writeLines('  </div>', out)
  writeLines('  <div class="cg"><label>Game</label>', out)
  writeLines('    <div class="cd-wrap" id="cdWrap">', out)
  writeLines('      <div class="cd-btn" id="cdBtn"><span id="cdLabel">Select a pitcher first</span><span class="cd-arrow">&#9660;</span></div>', out)
  writeLines('      <div class="cd-list" id="cdList"></div>', out)
  writeLines('    </div>', out)
  writeLines('  </div>', out)
  writeLines('  <div class="cg"><label>Pitch type</label>', out)
  writeLines('    <div class="cd-wrap" id="ptWrap">', out)
  writeLines('      <div class="cd-btn" id="ptBtn"><span id="ptLabel">Select a pitcher first</span><span class="cd-arrow">&#9660;</span></div>', out)
  writeLines('      <div class="cd-list" id="ptList"></div>', out)
  writeLines('    </div>', out)
  writeLines('  </div>', out)
  writeLines('  <div class="cg"><label>Result</label>', out)
  writeLines('    <div class="cd-wrap" id="rWrap">', out)
  writeLines('      <div class="cd-btn" id="rBtn"><span id="rLabel">All results</span><span class="cd-arrow">&#9660;</span></div>', out)
  writeLines('      <div class="cd-list" id="rList"></div>', out)
  writeLines('    </div>', out)
  writeLines('  </div>', out)
  writeLines('  <div class="toggle-wrap"><label>View</label>', out)
  writeLines('    <div class="toggle">', out)
  writeLines('      <button id="btnGame" class="active" onclick="setView(\'game\')">Game Progression</button>', out)
  writeLines('      <button id="btnOuting" onclick="setView(\'outing\')">Outing Progression</button>', out)
  writeLines('    </div>', out)
  writeLines('  </div>', out)
  writeLines('  <div class="cg" style="justify-content:flex-end">', out)
  writeLines('    <button class="clear-btn" onclick="clearAll()">Clear all</button>', out)
  writeLines('  </div>', out)
  writeLines('</div>', out)

  # STAT CARDS
  writeLines('<div class="cards">', out)
  writeLines('  <div class="card"><div class="lbl">Avg spin</div><div class="val" id="cAvg">&#8212;</div><div class="unit">RPM</div></div>', out)
  writeLines('  <div class="card"><div class="lbl">Peak spin</div><div class="val" id="cMax">&#8212;</div><div class="unit">RPM</div></div>', out)
  writeLines('  <div class="card"><div class="lbl">Pitches shown</div><div class="val" id="cCnt">&#8212;</div><div class="unit">shown</div></div>', out)
  writeLines('  <div class="card"><div class="lbl">Avg velocity</div><div class="val" id="cVelo">&#8212;</div><div class="unit">MPH</div></div>', out)
  writeLines('</div>', out)

  # CHART
  writeLines('<div class="cw"><canvas id="sc"></canvas></div>', out)
  writeLines('<div class="zoom-bar" id="zoomBar"><span id="zoomLabel"></span><button onclick="resetZoom()">&#8592; Reset zoom</button></div>', out)
  writeLines('<div class="leg" id="leg"></div>', out)
  writeLines('<div id="tt"><div id="ttn"></div><div id="ttb"></div></div>', out)

  # CHART.JS
  writeLines('<script>', out)
  writeLines(chartjs, out)
  writeLines('</script>', out)

  # DATA
  writeLines('<script>', out)
  writeLines(paste0('window.CARDINALS_DATA=', rows_json, ';'), out)
  writeLines(paste0('window.ROSTER_STATUS=', roster_json, ';'), out)
  writeLines('</script>', out)

  # MAIN JS
  writeLines('<script>', out)

  # Constants
  writeLines('const BY_PITCHER={},PITCHER_ID_MAP={};', out)
  writeLines('window.CARDINALS_DATA.forEach(function(r){if(!BY_PITCHER[r[0]])BY_PITCHER[r[0]]=[];BY_PITCHER[r[0]].push(r);if(r[14]&&!PITCHER_ID_MAP[r[0]])PITCHER_ID_MAP[r[0]]=r[14];});', out)
  writeLines('const COLORS={"4-Seam Fastball":"#2a78d6","Sinker":"#1baf7a","Slider":"#eda100","Sweeper":"#eb6834","Changeup":"#e34948","Curveball":"#4a3aa7","Cutter":"#e87ba4","Knuckle Curve":"#008300","Split-Finger":"#888780"};', out)
  writeLines('const DISPLAY={"Pallante, Andre":"Andre Pallante","Liberatore, Matthew":"Matthew Liberatore","McGreevy, Michael":"Michael McGreevy","May, Dustin":"Dustin May","Leahy, Kyle":"Kyle Leahy","Bruihl, Justin":"Justin Bruihl","Svanson, Matt":"Matt Svanson","Stanek, Ryne":"Ryne Stanek","Romero, JoJo":"JoJo Romero","Graceffo, Gordon":"Gordon Graceffo","O\'Brien, Riley":"Riley O\'Brien","Soriano, George":"George Soriano","Dobbins, Hunter":"Hunter Dobbins","Fernandez, Ryan":"Ryan Fernandez","Roycroft, Chris":"Chris Roycroft","Pushard, Matt":"Matt Pushard","Shuster, Jared":"Jared Shuster","Rajcic, Max":"Max Rajcic","Mautz, Brycen":"Brycen Mautz"};', out)
  writeLines('const WHIFF=["swinging_strike","swinging_strike_blocked","foul_tip","missed_bunt","bunt_foul_tip"];', out)
  writeLines('const INPLAY=["hit_into_play"];', out)
  writeLines('const BALLS=["ball","blocked_ball","pitchout"];', out)
  writeLines('const FOULS=["foul","foul_bunt","foul_pitchout"];', out)
  writeLines('const RESULT_OPTIONS=[{value:"called_strike",label:"Called strikes"},{value:"whiff",label:"Whiffs"},{value:"foul",label:"Fouls"},{value:"ball",label:"Balls"},{value:"walk",label:"Walk"},{value:"hbp",label:"HBP"},{value:"in_play",label:"In play"},{value:"DIVIDER",label:"---"},{value:"single",label:"Single"},{value:"double",label:"Double"},{value:"triple",label:"Triple"},{value:"home_run",label:"HR"},{value:"error",label:"Error"}];', out)

  # Helper functions
  writeLines('function toIP(v){return Math.round((v-1)*10000)/10000;}', out)
  writeLines('function fmtIP(v){var w=Math.floor(v+0.001),f=Math.round((v-w)*10000)/10000;if(Math.abs(v)<0.001)return"0";if(Math.abs(f)<0.02)return String(w);if(Math.abs(f-0.3333)<0.02)return w>0?w+"\u2153":"\u2153";if(Math.abs(f-0.6667)<0.02)return w>0?w+"\u2154":"\u2154";return v.toFixed(2);}', out)
  writeLines('function fmtOuts(v){var w=Math.floor(v+0.001),f=Math.round((v-w)*10000)/10000;if(Math.abs(f)<0.02)return"0";if(Math.abs(f-0.3333)<0.02)return"\u2153";if(Math.abs(f-0.6667)<0.02)return"\u2154";return"";}', out)
  writeLines('function fmtGameDate(d){var p=d.split("-");return parseInt(p[1])+"/"+parseInt(p[2])+"/"+p[0];}', out)
  writeLines('function ordinal(n){var s=["th","st","nd","rd"],v=n%100;return n+(s[(v-20)%10]||s[v]||s[0]);}', out)
  writeLines('function rg(d,ev){if(WHIFF.includes(d))return"whiff";if(d==="called_strike")return"called_strike";if(d==="hit_by_pitch")return"hbp";if(FOULS.includes(d))return"foul";if(BALLS.includes(d))return ev==="walk"?"walk":"ball";if(INPLAY.includes(d)){if(ev==="single")return"single";if(ev==="double")return"double";if(ev==="triple")return"triple";if(ev==="home_run")return"home_run";if(ev==="field_error")return"error";return"in_play";}return"other";}', out)
  writeLines('function getPitchTypes(p){return Array.from(new Set((BY_PITCHER[p]||[]).map(function(r){return r[1];}))).sort();}', out)

  # Roster status
  writeLines('function getPitcherStatus(p){var idRaw=PITCHER_ID_MAP[p];if(!idRaw)return"former";var id=Number(idRaw);var rs=window.ROSTER_STATUS;var a=(rs.active||[]).map(Number);var il=(rs.injured||[]).map(Number);var fm=(rs.forty_man||[]).map(Number);if(a.includes(id))return"active";if(il.includes(id))return"injured";if(fm.includes(id))return"fortyMan";return"former";}', out)

  # Custom dropdown builder
  writeLines('function buildDropdown(listId,allLabel,items,onChange){var list=document.getElementById(listId);list.innerHTML="";var selected=new Set();var allRow=document.createElement("div");allRow.className="cd-all";var allLeft=document.createElement("div");allLeft.className="cd-all-left";var allCb=document.createElement("input");allCb.type="checkbox";allCb.checked=true;var allLbl=document.createElement("label");allLbl.textContent=allLabel;allLbl.style.cursor="pointer";allLeft.append(allCb,allLbl);var resetBtn=document.createElement("button");resetBtn.className="cd-reset";resetBtn.textContent="Reset";resetBtn.addEventListener("click",function(e){e.stopPropagation();selected.clear();list.querySelectorAll("input.dd-cb").forEach(function(cb){cb.checked=false;});allCb.checked=true;onChange(selected);});allRow.append(allLeft,resetBtn);list.appendChild(allRow);allCb.addEventListener("change",function(){if(this.checked){selected.clear();list.querySelectorAll("input.dd-cb").forEach(function(cb){cb.checked=false;});onChange(selected);}else{this.checked=true;}});list.appendChild(Object.assign(document.createElement("hr"),{className:"cd-divider"}));items.forEach(function(item){if(item.value==="DIVIDER"){list.appendChild(Object.assign(document.createElement("hr"),{className:"cd-divider"}));return;}var row=document.createElement("div");row.className="cd-item";var cb=document.createElement("input");cb.type="checkbox";cb.className="dd-cb";cb.value=item.value;var lbl=document.createElement("label");lbl.textContent=item.label;lbl.style.cursor="pointer";cb.addEventListener("change",function(){if(this.checked){selected.add(this.value);allCb.checked=false;}else{selected.delete(this.value);if(selected.size===0)allCb.checked=true;}onChange(selected);});row.append(cb,lbl);list.appendChild(row);});return{getSelected:function(){return selected.size===0?null:new Set(selected);},reset:function(){selected.clear();list.querySelectorAll("input.dd-cb").forEach(function(cb){cb.checked=false;});allCb.checked=true;}};}', out)

  writeLines('function updateLabel(id,sel,allText,singleFmt){var lbl=document.getElementById(id);if(!sel||sel.size===0)lbl.textContent=allText;else if(sel.size===1)lbl.textContent=singleFmt(Array.from(sel)[0]);else lbl.textContent=sel.size+" selected";}', out)

  # Close dropdowns on outside click
  writeLines('window.addEventListener("click",function(e){[["cdWrap","cdList"],["ptWrap","ptList"],["rWrap","rList"]].forEach(function(pair){var wrap=document.getElementById(pair[0]);var list=document.getElementById(pair[1]);if(wrap&&list&&!wrap.contains(e.target))list.classList.remove("open");});});', out)
  writeLines('document.getElementById("cdBtn").addEventListener("click",function(){document.getElementById("cdList").classList.toggle("open");});', out)
  writeLines('document.getElementById("ptBtn").addEventListener("click",function(){document.getElementById("ptList").classList.toggle("open");});', out)
  writeLines('document.getElementById("rBtn").addEventListener("click",function(){document.getElementById("rList").classList.toggle("open");});', out)

  # State
  writeLines('var chart=null,currentView="game",zoomedInning=null,gameDD=null,ptDD=null,rDD=null;', out)

  # Zoom
  writeLines('function resetZoom(){zoomedInning=null;document.getElementById("zoomBar").style.display="none";render();}', out)
  writeLines('function setZoom(inn){zoomedInning=inn;var label=currentView==="outing"?inn+" IP":(isMobile?ordinal(inn):ordinal(inn)+" inning");document.getElementById("zoomLabel").textContent="Zoomed: "+label;document.getElementById("zoomBar").style.display="flex";render();}', out)

  # View toggle
  writeLines('function setView(v){currentView=v;zoomedInning=null;document.getElementById("zoomBar").style.display="none";document.getElementById("btnGame").classList.toggle("active",v==="game");document.getElementById("btnOuting").classList.toggle("active",v==="outing");render();}', out)

  # Clear all
  writeLines('function clearAll(){document.getElementById("pSel").value="";if(gameDD)gameDD.reset();if(ptDD)ptDD.reset();if(rDD)rDD.reset();document.getElementById("cdLabel").textContent="Select a pitcher first";document.getElementById("ptLabel").textContent="Select a pitcher first";document.getElementById("rLabel").textContent="All results";document.getElementById("cdBtn").style.pointerEvents="none";document.getElementById("cdBtn").style.opacity="0.45";document.getElementById("ptBtn").style.pointerEvents="none";document.getElementById("ptBtn").style.opacity="0.45";zoomedInning=null;document.getElementById("zoomBar").style.display="none";currentView="game";document.getElementById("btnGame").classList.add("active");document.getElementById("btnOuting").classList.remove("active");if(chart){chart.destroy();chart=null;}["cAvg","cMax","cCnt","cVelo"].forEach(function(id){document.getElementById(id).textContent="\u2014";});document.getElementById("leg").innerHTML="";}', out)

  # Init dropdowns
  writeLines('function initDropdowns(player){document.getElementById("cdBtn").style.pointerEvents="";document.getElementById("cdBtn").style.opacity="";document.getElementById("ptBtn").style.pointerEvents="";document.getElementById("ptBtn").style.opacity="";var rows=BY_PITCHER[player]||[];var gameMap=new Map();rows.forEach(function(r){if(!gameMap.has(r[8]))gameMap.set(r[8],{opponent:r[11],homeAway:r[12]});});var gameItems=Array.from(gameMap.keys()).sort().map(function(date){var info=gameMap.get(date);return{value:date,label:fmtGameDate(date)+" ("+info.opponent+" - "+info.homeAway+")"};});gameDD=buildDropdown("cdList","All games",gameItems,function(sel){updateLabel("cdLabel",sel,"All games",function(v){var info=gameMap.get(v);return fmtGameDate(v)+(info?" ("+info.opponent+" - "+info.homeAway+")":"");});zoomedInning=null;document.getElementById("zoomBar").style.display="none";render();});var ptItems=getPitchTypes(player).map(function(pt){return{value:pt,label:pt};});ptDD=buildDropdown("ptList","All pitches",ptItems,function(sel){updateLabel("ptLabel",sel,"All pitches",function(v){return v;});render();});document.getElementById("cdLabel").textContent="All games";document.getElementById("ptLabel").textContent="All pitches";}', out)

  # Tick values
  writeLines('var TICK_VALUES=[0];for(var _i=0;_i<=11;_i++){TICK_VALUES.push(+(_i+1/3).toFixed(4));TICK_VALUES.push(+(_i+2/3).toFixed(4));TICK_VALUES.push(_i+1);}', out)

  # Custom axis plugin
  writeLines('var customAxisPlugin={id:"customAxis",afterDraw:function(chart){var ctx=chart.ctx,x=chart.scales.x,y=chart.scales.y;ctx.save();for(var inn=1;inn<=12;inn++){var lineX=inn-(1/6);if(lineX<x.min||lineX>x.max)continue;var xPx=x.getPixelForValue(lineX);var zoneStart=(inn-1)-(1/6)<x.min?x.min:(inn-1)-(1/6);var midX=x.getPixelForValue((zoneStart+lineX)/2);var label=currentView==="outing"?inn+" IP":(isMobile?ordinal(inn):ordinal(inn)+" inning");var isZoomed=zoomedInning===inn;if(isZoomed){ctx.setLineDash([]);ctx.fillStyle="rgba(42,120,214,0.07)";ctx.fillRect(x.getPixelForValue(zoneStart),y.top,xPx-x.getPixelForValue(zoneStart),y.bottom-y.top);}ctx.setLineDash([4,4]);ctx.strokeStyle="rgba(0,0,0,0.2)";ctx.lineWidth=1;ctx.beginPath();ctx.moveTo(xPx,y.top);ctx.lineTo(xPx,y.bottom);ctx.stroke();ctx.setLineDash([]);ctx.font="bold 10px system-ui,sans-serif";ctx.textAlign="center";var tw=ctx.measureText(label).width+14,th=15,tx=midX-tw/2,ty=y.top-17;ctx.fillStyle=isZoomed?"#2a78d6":"rgba(0,0,0,0.07)";if(ctx.roundRect){ctx.beginPath();ctx.roundRect(tx,ty,tw,th,4);ctx.fill();}else{ctx.fillRect(tx,ty,tw,th);}ctx.fillStyle=isZoomed?"#fff":"rgba(0,0,0,0.4)";ctx.fillText(label,midX,y.top-6);}ctx.setLineDash([]);ctx.fillStyle="#898781";ctx.font="11px system-ui,sans-serif";ctx.textAlign="center";var tickY=y.bottom+14;TICK_VALUES.forEach(function(v){if(v<x.min||v>x.max+0.01)return;var xPx=x.getPixelForValue(v);if(xPx<x.left-2||xPx>x.right+2)return;var lbl=fmtOuts(v);if(!lbl)return;ctx.strokeStyle="rgba(0,0,0,0.2)";ctx.lineWidth=1;ctx.beginPath();ctx.moveTo(xPx,y.bottom);ctx.lineTo(xPx,y.bottom+4);ctx.stroke();ctx.fillStyle="#898781";ctx.fillText(lbl,xPx,tickY);});ctx.restore();}};', out)

  # Render function
  writeLines('function render(){if(!gameDD||!ptDD||!rDD)return;var player=document.getElementById("pSel").value;if(!player)return;var selGames=gameDD.getSelected(),selPT=ptDD.getSelected(),selR=rDD.getSelected();var isOuting=currentView==="outing";var isMobile=window.innerWidth<=768;var jitter=isMobile?0.12:0.02;var ptRadius=isMobile?1.5:3.5;var ptHover=isMobile?4:6;var allRows=BY_PITCHER[player]||[];var rows=selGames===null?allRows:allRows.filter(function(r){return selGames.has(r[8]);});var allTypes=getPitchTypes(player);var types=selPT===null?allTypes:allTypes.filter(function(t){return selPT.has(t);});var datasets=types.map(function(pt){var pts=rows.filter(function(r){return r[1]===pt&&(selR===null||selR.has(rg(r[7],r[13])));});return{label:pt,data:pts.map(function(r){return{x:toIP(isOuting?r[10]:r[2])+(Math.random()-0.5)*jitter,y:r[5],raw:r};}),backgroundColor:(COLORS[pt]||"#888")+"bb",borderColor:COLORS[pt]||"#888",borderWidth:.5,pointRadius:ptRadius,pointHoverRadius:ptHover};});var allPts=datasets.reduce(function(a,d){return a.concat(d.data);},[]);if(allPts.length){var spins=allPts.map(function(p){return p.y;}),velos=allPts.map(function(p){return p.raw[6];});document.getElementById("cAvg").textContent=Math.round(spins.reduce(function(a,b){return a+b;})/spins.length).toLocaleString();document.getElementById("cMax").textContent=Math.max.apply(null,spins).toLocaleString();document.getElementById("cCnt").textContent=allPts.length.toLocaleString();document.getElementById("cVelo").textContent=(velos.reduce(function(a,b){return a+b;})/velos.length).toFixed(1);}else{["cAvg","cMax","cCnt","cVelo"].forEach(function(id){document.getElementById(id).textContent="\u2014";});}var leg=document.getElementById("leg");leg.innerHTML="";types.forEach(function(pt){var cnt=rows.filter(function(r){return r[1]===pt;}).length;var div=document.createElement("div");div.className="li";var dot=document.createElement("div");dot.className="dot";dot.style.background=COLORS[pt]||"#888";var lbl=document.createElement("span");lbl.textContent=pt;var c=document.createElement("span");c.style.cssText="color:#898781;font-size:11px";c.textContent=" ("+cnt+")";div.append(dot,lbl,c);leg.appendChild(div);});if(chart)chart.destroy();var allPX=rows.map(function(r){return toIP(isOuting?r[10]:r[2]);});var pMax=allPX.length?Math.max.apply(null,allPX):8,pMin=allPX.length?Math.min.apply(null,allPX):0;var fullXMax=Math.ceil(pMax)+0.5,fullXMin=isOuting?0:Math.floor(pMin);var xMin=fullXMin,xMax=fullXMax;if(zoomedInning!==null){xMin=Math.max(fullXMin,(zoomedInning-1)-(1/6)-0.05);xMax=Math.min(fullXMax,zoomedInning-(1/6)+0.05);}var yTickFont=isMobile?8:11;chart=new Chart(document.getElementById("sc").getContext("2d"),{type:"scatter",data:{datasets:datasets},plugins:[customAxisPlugin],options:{responsive:true,maintainAspectRatio:false,animation:{duration:150},layout:{padding:{top:24,right:14,bottom:10}},scales:{x:{type:"linear",min:xMin-0.15,max:xMax,title:{display:true,text:isOuting?"Outs (outing progression)":"Outs",color:"#898781",font:{size:12},padding:{top:18}},ticks:{display:false},grid:{color:"rgba(0,0,0,.04)"}},y:{title:{display:true,text:"Spin Rate (RPM)",color:"#898781",font:{size:isMobile?9:12},display:!isMobile},ticks:{color:"#898781",font:{size:yTickFont},callback:function(v){if(v===3500)return"";if(isMobile&&v%1000!==0&&v!==600)return"";return v.toLocaleString();}},grid:{color:"rgba(0,0,0,.06)"},min:600,max:3600}},plugins:{legend:{display:false},tooltip:{enabled:false}},onHover:function(e,els){var tt=document.getElementById("tt");if(!els.length){tt.style.opacity=0;return;}var r=datasets[els[0].datasetIndex].data[els[0].index].raw;document.getElementById("ttn").textContent=r[1];document.getElementById("ttb").innerHTML="Spin: <b>"+r[5].toLocaleString()+" RPM</b><br>"+"Velo: "+r[6].toFixed(1)+" MPH<br>"+"Game IP: "+fmtIP(toIP(r[2]))+(isOuting?"<br>Outing IP: "+fmtIP(toIP(r[10])):"")+  "<br>Result: "+(r[13]&&r[13]!==""?r[13].replace(/_/g," "):r[7].replace(/_/g," "))+"<br>"+fmtGameDate(r[8])+" vs "+r[11]+" ("+r[12]+")";tt.style.left=(e.native.clientX+14)+"px";tt.style.top=(e.native.clientY-20)+"px";tt.style.opacity=1;}}});var numInnings=Math.ceil(xMax-fullXMin);var containerW=document.getElementById("sc").parentElement.clientWidth;var minPerInning=isMobile?200:Math.max(80,Math.floor(containerW/numInnings));var minCW=Math.max(containerW,numInnings*minPerInning);var canvas=document.getElementById("sc");canvas.style.minWidth=minCW+"px";chart.resize();if(!canvas.dataset.listenersAttached){canvas.dataset.listenersAttached="true";canvas.style.cursor="pointer";canvas.addEventListener("mouseleave",function(){document.getElementById("tt").style.opacity=0;});canvas.addEventListener("click",function(e){if(!chart)return;var rect=this.getBoundingClientRect();var mouseX=e.clientX-rect.left,mouseY=e.clientY-rect.top;var xS=chart.scales.x,yS=chart.scales.y;if(mouseY>yS.top)return;for(var inn=1;inn<=12;inn++){var lineX=inn-(1/6);if(lineX<xS.min||lineX>xS.max)continue;var zoneStart=(inn-1)-(1/6)<xS.min?xS.min:(inn-1)-(1/6);var startPx=xS.getPixelForValue(zoneStart),endPx=xS.getPixelForValue(lineX);if(mouseX>=startPx&&mouseX<=endPx){if(zoomedInning===inn){resetZoom();}else{setZoom(inn);}return;}}});}}', out)

  # Pitcher dropdown with roster tiers
  writeLines('var pitchers=Object.keys(BY_PITCHER).sort(function(a,b){return(DISPLAY[a]||a).localeCompare(DISPLAY[b]||b);});', out)
  writeLines('var pSel=document.getElementById("pSel");', out)
  writeLines('var defaultOpt=document.createElement("option");defaultOpt.value="";defaultOpt.textContent="Select Pitcher";defaultOpt.disabled=true;defaultOpt.selected=true;pSel.appendChild(defaultOpt);', out)

  writeLines('var STATUS_BG={active:"#ffffff",injured:"#ffd6d6",fortyMan:"#d6deff",former:"#e8e8e8"};', out)
  writeLines('var STATUS_COLOR={active:"#0b0b0b",injured:"#7a1f1f",fortyMan:"#1a2d5a",former:"#4a4a4a"};', out)

  writeLines('function addDivider(label){var opt=document.createElement("option");opt.disabled=true;opt.textContent="\u2500\u2500 "+label+" \u2500\u2500";opt.style.cssText="font-weight:600;color:#52514e;background:#f1efe8;";pSel.appendChild(opt);}', out)
  writeLines('function addPitcherOpts(list,status){list.forEach(function(p){var o=document.createElement("option");o.value=p;o.textContent=DISPLAY[p]||p;o.style.background=STATUS_BG[status]||"#fff";o.style.color=STATUS_COLOR[status]||"#0b0b0b";pSel.appendChild(o);});}', out)

  writeLines('var grouped={active:[],injured:[],fortyMan:[],former:[]};', out)
  writeLines('pitchers.forEach(function(p){var s=getPitcherStatus(p);if(grouped[s]){grouped[s].push(p);}else{grouped.former.push(p);}});', out)

  writeLines('addDivider("Active Roster");addPitcherOpts(grouped.active,"active");', out)
  writeLines('if(grouped.injured.length){addDivider("IL / Leave List");addPitcherOpts(grouped.injured,"injured");}', out)
  writeLines('if(grouped.fortyMan.length){addDivider("40-Man Roster");addPitcherOpts(grouped.fortyMan,"fortyMan");}', out)
  writeLines('if(grouped.former.length){addDivider("Former Cardinals");addPitcherOpts(grouped.former,"former");}', out)

  writeLines('pSel.addEventListener("change",function(){if(!pSel.value)return;zoomedInning=null;document.getElementById("zoomBar").style.display="none";initDropdowns(pSel.value);render();});', out)

  # Result dropdown
  writeLines('rDD=buildDropdown("rList","All results",RESULT_OPTIONS,function(sel){updateLabel("rLabel",sel,"All results",function(v){var found=RESULT_OPTIONS.find(function(o){return o.value===v;});return found?found.label:v;});render();});', out)

  # Disable game/pitch type until pitcher selected
  writeLines('gameDD={getSelected:function(){return null;},reset:function(){}};', out)
  writeLines('ptDD={getSelected:function(){return null;},reset:function(){}};', out)
  writeLines('document.getElementById("cdBtn").style.pointerEvents="none";', out)
  writeLines('document.getElementById("cdBtn").style.opacity="0.45";', out)
  writeLines('document.getElementById("ptBtn").style.pointerEvents="none";', out)
  writeLines('document.getElementById("ptBtn").style.opacity="0.45";', out)

  writeLines('</script>', out)
  writeLines('</body>', out)
  writeLines('</html>', out)

  close(out)
  message(sprintf("Dashboard written: %s (%.0f KB)", output_html, file.size(output_html)/1024))
  invisible(output_html)
}
