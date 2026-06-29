# =============================================================================
# build_dashboard.R
#
# ONE FILE — no template needed.
# Reads cardinals_clean_LATEST.csv and writes cardinals_dashboard.html.
#
# Usage:
#   setwd("C:/Users/joshh/OneDrive/Desktop/Coding Projects/cardinals")
#   source("build_dashboard.R")
#   build_cardinals_dashboard()
# =============================================================================

library(dplyr)
library(jsonlite)

build_cardinals_dashboard <- function(
    clean_csv      = "cardinals_data/cardinals_clean_LATEST.csv",
    output_html    = "cardinals_dashboard.html",
    roster_status  = NULL
) {

  message("Reading: ", clean_csv)
  df <- read.csv(clean_csv, stringsAsFactors = FALSE)

  # Scrub
  depr <- grep("deprecated", names(df), value = TRUE)
  if (length(depr) > 0) df <- df[, !names(df) %in% depr]
  df <- df[!duplicated(df[, c("game_pk","at_bat_number","pitch_number","pitcher","batter")]), ]
  df <- df[!df$description %in% c("automatic_ball","intent_walked"), ]
  df <- df[!df$pitch_name  %in% c("Pitch Out","Intentional Ball"), ]
  df <- df[!is.na(df$release_spin_rate) & !is.na(df$release_speed) & !is.na(df$pitch_name), ]
  df <- df[df$release_spin_rate >= 100 & df$release_spin_rate <= 4000, ]
  df$inning_numeric    <- round(df$inning + df$outs_when_up / 3, 4)
  df$release_spin_rate <- as.integer(round(df$release_spin_rate))
  df$release_speed     <- round(df$release_speed, 1)
  df$game_date         <- as.character(df$game_date)
  df$events            <- ifelse(is.na(df$events), "", as.character(df$events))
  df$is_home           <- df$home_team == "STL"
  df$opponent          <- ifelse(df$is_home, df$away_team, df$home_team)
  df$home_away         <- ifelse(df$is_home, "Home", "Away")
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
  cols <- c("player_name","pitch_name","inning_numeric","inning","outs_when_up",
            "release_spin_rate","release_speed","description","game_date",
            "game_pk","outing_progression","opponent","home_away","events","pitcher")
  # Only keep pitcher column if it exists
  cols <- cols[cols %in% names(df)]
  df <- df[, cols]
  n_pitches  <- nrow(df)
  n_pitchers <- length(unique(df$player_name))
  message(sprintf("Scrubbed: %d pitches, %d pitchers", n_pitches, n_pitchers))

  # Serialize pitch data to JSON
  rows_json <- as.character(toJSON(
    lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, ]))),
    auto_unbox = TRUE
  ))

  # Build roster status map: player_name -> "active"|"injured"|"40man"|"former"
  # Get all unique pitcher names and their IDs from the data
  pitcher_status <- list()
  if (!is.null(roster_status)) {
    # We need to map pitcher IDs to names — use the pitcher column if available
    # Build status by checking each unique pitcher in the data
    unique_pitchers <- unique(df$player_name)
    for (pname in unique_pitchers) {
      # Default to former unless we find them on a roster
      pitcher_status[[pname]] <- "former"
    }
    # We can't easily map names to IDs here without the raw data
    # So we pass the ID lists and do the mapping in JS using game_pk lookup
    # Instead, serialize the ID lists and let the dashboard match via pitcher column
  }

  # Serialize roster status ID lists for JS
  if (!is.null(roster_status)) {
    roster_json <- as.character(toJSON(list(
      active    = as.integer(roster_status$active),
      injured   = as.integer(roster_status$injured),
      forty_man = as.integer(roster_status$forty_man)
    ), auto_unbox = FALSE))
  } else {
    roster_json <- '{"active":[],"injured":[],"forty_man":[]}'
  }

  # Download Chart.js once and cache it
  chartjs_cache <- "chart.umd.js"
  if (!file.exists(chartjs_cache)) {
    message("Downloading Chart.js (one-time)...")
    download.file(
      "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js",
      chartjs_cache, quiet = TRUE
    )
  }
  chartjs <- paste(readLines(chartjs_cache, encoding = "UTF-8", warn = FALSE), collapse = "\n")

  # Write dashboard HTML line by line
  out <- file(output_html, open = "wt", encoding = "UTF-8")

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
  writeLines('.cw{position:relative;overflow-x:auto;width:100%;height:60vh;max-height:600px;min-height:280px;margin-bottom:.5rem}', out)
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
  writeLines('</style>', out)
  writeLines('</head>', out)
  writeLines('<body>', out)
  writeLines('<h1>St. Louis Cardinals 2026 Pitching Staff Spin Rate Analyzer</h1>', out)
  writeLines('<div class="controls">', out)
  writeLines('  <div class="cg"><label>Pitcher</label>', out)
  writeLines('    <select id="pSel" style="height:36px;padding:0 10px;border-radius:8px;border:1px solid #ccc;background:#fff;color:#0b0b0b;font-size:13px;cursor:pointer;min-width:175px"></select>', out)
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
  writeLines('<div class="cards">', out)
  writeLines('  <div class="card"><div class="lbl">Avg spin</div><div class="val" id="cAvg">&#8212;</div><div class="unit">RPM</div></div>', out)
  writeLines('  <div class="card"><div class="lbl">Peak spin</div><div class="val" id="cMax">&#8212;</div><div class="unit">RPM</div></div>', out)
  writeLines('  <div class="card"><div class="lbl">Pitches shown</div><div class="val" id="cCnt">&#8212;</div><div class="unit">shown</div></div>', out)
  writeLines('  <div class="card"><div class="lbl">Avg velocity</div><div class="val" id="cVelo">&#8212;</div><div class="unit">MPH</div></div>', out)
  writeLines('</div>', out)
  writeLines('<div class="cw"><canvas id="sc"></canvas></div>', out)
  writeLines('<div class="zoom-bar" id="zoomBar"><span id="zoomLabel"></span><button onclick="resetZoom()">&#8592; Reset zoom</button></div>', out)
  writeLines('<div class="leg" id="leg"></div>', out)
  writeLines('<div id="tt"><div id="ttn"></div><div id="ttb"></div></div>', out)

  # Embed Chart.js
  writeLines('<script>', out)
  writeLines(chartjs, out)
  writeLines('</script>', out)

  # Embed data
  writeLines('<script>', out)
  writeLines(paste0('window.CARDINALS_DATA=', rows_json, ';'), out)
  writeLines(paste0('window.ROSTER_STATUS=', roster_json, ';'), out)
  writeLines('</script>', out)

  # Main JS
  writeLines('<script>', out)
  writeLines('const BY_PITCHER={};', out)
  writeLines('const PITCHER_ID_MAP={};', out)
  writeLines('window.CARDINALS_DATA.forEach(r=>{if(!BY_PITCHER[r[0]])BY_PITCHER[r[0]]=[];BY_PITCHER[r[0]].push(r);if(r[14]&&!PITCHER_ID_MAP[r[0]])PITCHER_ID_MAP[r[0]]=r[14];});', out)
  writeLines('const COLORS={"4-Seam Fastball":"#2a78d6","Sinker":"#1baf7a","Slider":"#eda100","Sweeper":"#eb6834","Changeup":"#e34948","Curveball":"#4a3aa7","Cutter":"#e87ba4","Knuckle Curve":"#008300","Split-Finger":"#888780"};', out)
  writeLines('const DISPLAY={"Pallante, Andre":"Andre Pallante","Liberatore, Matthew":"Matthew Liberatore","McGreevy, Michael":"Michael McGreevy","May, Dustin":"Dustin May","Leahy, Kyle":"Kyle Leahy","Bruihl, Justin":"Justin Bruihl","Svanson, Matt":"Matt Svanson","Stanek, Ryne":"Ryne Stanek","Romero, JoJo":"JoJo Romero","Graceffo, Gordon":"Gordon Graceffo","O\'Brien, Riley":"Riley O\'Brien","Soriano, George":"George Soriano","Dobbins, Hunter":"Hunter Dobbins","Fernandez, Ryan":"Ryan Fernandez","Roycroft, Chris":"Chris Roycroft","Pushard, Matt":"Matt Pushard","Shuster, Jared":"Jared Shuster","Rajcic, Max":"Max Rajcic","Mautz, Brycen":"Brycen Mautz"};', out)
  writeLines('const WHIFF=["swinging_strike","swinging_strike_blocked","foul_tip","missed_bunt","bunt_foul_tip"];', out)
  writeLines('const INPLAY=["hit_into_play"];', out)
  writeLines('const BALLS=["ball","blocked_ball","pitchout"];', out)
  writeLines('const FOULS=["foul","foul_bunt","foul_pitchout"];', out)
  writeLines('const RESULT_OPTIONS=[{value:"called_strike",label:"Called strikes"},{value:"whiff",label:"Whiffs"},{value:"foul",label:"Fouls"},{value:"ball",label:"Balls"},{value:"walk",label:"Walk"},{value:"hbp",label:"HBP"},{value:"in_play",label:"In play"},{value:"DIVIDER",label:"---"},{value:"single",label:"Single"},{value:"double",label:"Double"},{value:"triple",label:"Triple"},{value:"home_run",label:"HR"},{value:"error",label:"Error"}];', out)
  writeLines('function toIP(v){return Math.round((v-1)*10000)/10000;}', out)
  writeLines('function fmtIP(v){const w=Math.floor(v+0.001),f=Math.round((v-w)*10000)/10000;if(Math.abs(v)<0.001)return"0";if(Math.abs(f)<0.02)return String(w);if(Math.abs(f-0.3333)<0.02)return w>0?w+"\u2153":"\u2153";if(Math.abs(f-0.6667)<0.02)return w>0?w+"\u2154":"\u2154";return v.toFixed(2);}', out)
  writeLines('function fmtOuts(v){const w=Math.floor(v+0.001),f=Math.round((v-w)*10000)/10000;if(Math.abs(f)<0.02)return"0";if(Math.abs(f-0.3333)<0.02)return"\u2153";if(Math.abs(f-0.6667)<0.02)return"\u2154";return"";}', out)
  writeLines('function fmtGameDate(d){const[y,m,day]=d.split("-");return parseInt(m)+"/"+parseInt(day)+"/"+y;}', out)
  writeLines('function ordinal(n){const s=["th","st","nd","rd"],v=n%100;return n+(s[(v-20)%10]||s[v]||s[0]);}', out)
  writeLines('function rg(d,ev){if(WHIFF.includes(d))return"whiff";if(d==="called_strike")return"called_strike";if(d==="hit_by_pitch")return"hbp";if(FOULS.includes(d))return"foul";if(BALLS.includes(d))return ev==="walk"?"walk":"ball";if(INPLAY.includes(d)){if(ev==="single")return"single";if(ev==="double")return"double";if(ev==="triple")return"triple";if(ev==="home_run")return"home_run";if(ev==="field_error")return"error";return"in_play";}return"other";}', out)
  writeLines('function getPitchTypes(player){return Array.from(new Set((BY_PITCHER[player]||[]).map(r=>r[1]))).sort();}', out)
  writeLines('function buildDropdown(listId,allLabel,items,onChange){const list=document.getElementById(listId);list.innerHTML="";const selected=new Set();const allRow=document.createElement("div");allRow.className="cd-all";const allLeft=document.createElement("div");allLeft.className="cd-all-left";const allCb=document.createElement("input");allCb.type="checkbox";allCb.checked=true;const allLbl=document.createElement("label");allLbl.textContent=allLabel;allLbl.style.cursor="pointer";allLeft.append(allCb,allLbl);const resetBtn=document.createElement("button");resetBtn.className="cd-reset";resetBtn.textContent="Reset";resetBtn.addEventListener("click",function(e){e.stopPropagation();selected.clear();list.querySelectorAll("input.dd-cb").forEach(cb=>cb.checked=false);allCb.checked=true;onChange(selected);});allRow.append(allLeft,resetBtn);list.appendChild(allRow);allCb.addEventListener("change",function(){if(this.checked){selected.clear();list.querySelectorAll("input.dd-cb").forEach(cb=>cb.checked=false);onChange(selected);}else{this.checked=true;}});list.appendChild(Object.assign(document.createElement("hr"),{className:"cd-divider"}));items.forEach(function(item){if(item.value==="DIVIDER"){list.appendChild(Object.assign(document.createElement("hr"),{className:"cd-divider"}));return;}const row=document.createElement("div");row.className="cd-item";const cb=document.createElement("input");cb.type="checkbox";cb.className="dd-cb";cb.value=item.value;const lbl=document.createElement("label");lbl.textContent=item.label;lbl.style.cursor="pointer";cb.addEventListener("change",function(){if(this.checked){selected.add(this.value);allCb.checked=false;}else{selected.delete(this.value);if(selected.size===0)allCb.checked=true;}onChange(selected);});row.append(cb,lbl);list.appendChild(row);});return{getSelected:function(){return selected.size===0?null:new Set(selected);},reset:function(){selected.clear();list.querySelectorAll("input.dd-cb").forEach(cb=>cb.checked=false);allCb.checked=true;}};}', out)
  writeLines('function updateLabel(id,sel,allText,singleFmt){const lbl=document.getElementById(id);if(!sel||sel.size===0)lbl.textContent=allText;else if(sel.size===1)lbl.textContent=singleFmt(Array.from(sel)[0]);else lbl.textContent=sel.size+" selected";}', out)
  writeLines('window.addEventListener("click",function(e){[["cdWrap","cdList"],["ptWrap","ptList"],["rWrap","rList"]].forEach(function(pair){const wrap=document.getElementById(pair[0]);const list=document.getElementById(pair[1]);if(wrap&&list&&!wrap.contains(e.target))list.classList.remove("open");});});', out)
  writeLines('document.getElementById("cdBtn").addEventListener("click",function(){document.getElementById("cdList").classList.toggle("open");});', out)
  writeLines('document.getElementById("ptBtn").addEventListener("click",function(){document.getElementById("ptList").classList.toggle("open");});', out)
  writeLines('document.getElementById("rBtn").addEventListener("click",function(){document.getElementById("rList").classList.toggle("open");});', out)
  writeLines('let chart=null,currentView="game",zoomedInning=null,gameDD=null,ptDD=null,rDD=null;', out)
  writeLines('function resetZoom(){zoomedInning=null;document.getElementById("zoomBar").style.display="none";render();}', out)
  writeLines('function setZoom(inn){zoomedInning=inn;const label=currentView==="outing"?inn+" IP":ordinal(inn)+" inning";document.getElementById("zoomLabel").textContent="Zoomed: "+label;document.getElementById("zoomBar").style.display="flex";render();}', out)
  writeLines('function setView(v){currentView=v;zoomedInning=null;document.getElementById("zoomBar").style.display="none";document.getElementById("btnGame").classList.toggle("active",v==="game");document.getElementById("btnOuting").classList.toggle("active",v==="outing");render();}', out)
  writeLines('function clearAll(){document.getElementById("pSel").value="";if(gameDD)gameDD.reset();if(ptDD)ptDD.reset();if(rDD)rDD.reset();document.getElementById("cdLabel").textContent="Select a pitcher first";document.getElementById("ptLabel").textContent="Select a pitcher first";document.getElementById("rLabel").textContent="All results";document.getElementById("cdBtn").style.pointerEvents="none";document.getElementById("cdBtn").style.opacity="0.45";document.getElementById("ptBtn").style.pointerEvents="none";document.getElementById("ptBtn").style.opacity="0.45";zoomedInning=null;document.getElementById("zoomBar").style.display="none";currentView="game";document.getElementById("btnGame").classList.add("active");document.getElementById("btnOuting").classList.remove("active");if(chart){chart.destroy();chart=null;}["cAvg","cMax","cCnt","cVelo"].forEach(id=>document.getElementById(id).textContent="\u2014");document.getElementById("leg").innerHTML="";}', out)
  writeLines('function initDropdowns(player){document.getElementById("cdBtn").style.pointerEvents="";document.getElementById("cdBtn").style.opacity="";document.getElementById("ptBtn").style.pointerEvents="";document.getElementById("ptBtn").style.opacity="";const rows=BY_PITCHER[player]||[];const gameMap=new Map();rows.forEach(r=>{if(!gameMap.has(r[8]))gameMap.set(r[8],{opponent:r[11],homeAway:r[12]});});const gameItems=Array.from(gameMap.keys()).sort().map(date=>{const info=gameMap.get(date);return{value:date,label:fmtGameDate(date)+" ("+info.opponent+" - "+info.homeAway+")"};});gameDD=buildDropdown("cdList","All games",gameItems,function(sel){updateLabel("cdLabel",sel,"All games",function(v){const info=gameMap.get(v);return fmtGameDate(v)+(info?" ("+info.opponent+" - "+info.homeAway+")":"");});zoomedInning=null;document.getElementById("zoomBar").style.display="none";render();});const ptItems=getPitchTypes(player).map(pt=>({value:pt,label:pt}));ptDD=buildDropdown("ptList","All pitches",ptItems,function(sel){updateLabel("ptLabel",sel,"All pitches",function(v){return v;});render();});document.getElementById("cdLabel").textContent="All games";document.getElementById("ptLabel").textContent="All pitches";}', out)
  writeLines('const TICK_VALUES=[0];for(let i=0;i<=11;i++){TICK_VALUES.push(+(i+1/3).toFixed(4));TICK_VALUES.push(+(i+2/3).toFixed(4));TICK_VALUES.push(i+1);}', out)
  writeLines('const customAxisPlugin={id:"customAxis",afterDraw(chart){const ctx=chart.ctx,x=chart.scales.x,y=chart.scales.y;ctx.save();for(let inn=1;inn<=12;inn++){const lineX=inn-(1/6);if(lineX<x.min||lineX>x.max)continue;const xPx=x.getPixelForValue(lineX);const zoneStart=(inn-1)-(1/6)<x.min?x.min:(inn-1)-(1/6);const midX=x.getPixelForValue((zoneStart+lineX)/2);const label=currentView==="outing"?inn+" IP":ordinal(inn)+" inning";const isZoomed=zoomedInning===inn;if(isZoomed){ctx.setLineDash([]);ctx.fillStyle="rgba(42,120,214,0.07)";ctx.fillRect(x.getPixelForValue(zoneStart),y.top,xPx-x.getPixelForValue(zoneStart),y.bottom-y.top);}ctx.setLineDash([4,4]);ctx.strokeStyle="rgba(0,0,0,0.2)";ctx.lineWidth=1;ctx.beginPath();ctx.moveTo(xPx,y.top);ctx.lineTo(xPx,y.bottom);ctx.stroke();ctx.setLineDash([]);ctx.font="bold 10px system-ui,sans-serif";ctx.textAlign="center";const tw=ctx.measureText(label).width+14,th=15,tx=midX-tw/2,ty=y.top-17;ctx.fillStyle=isZoomed?"#2a78d6":"rgba(0,0,0,0.07)";if(ctx.roundRect){ctx.beginPath();ctx.roundRect(tx,ty,tw,th,4);ctx.fill();}else{ctx.fillRect(tx,ty,tw,th);}ctx.fillStyle=isZoomed?"#fff":"rgba(0,0,0,0.4)";ctx.fillText(label,midX,y.top-6);}ctx.setLineDash([]);ctx.fillStyle="#898781";ctx.font="11px system-ui,sans-serif";ctx.textAlign="center";const tickY=y.bottom+14;TICK_VALUES.forEach(v=>{if(v<x.min||v>x.max+0.01)return;const xPx=x.getPixelForValue(v);if(xPx<x.left-2||xPx>x.right+2)return;const lbl=fmtOuts(v);if(!lbl)return;ctx.strokeStyle="rgba(0,0,0,0.2)";ctx.lineWidth=1;ctx.beginPath();ctx.moveTo(xPx,y.bottom);ctx.lineTo(xPx,y.bottom+4);ctx.stroke();ctx.fillStyle="#898781";ctx.fillText(lbl,xPx,tickY);});ctx.restore();}};', out)
  writeLines('function render(){if(!gameDD||!ptDD||!rDD)return;const player=document.getElementById("pSel").value;if(!player)return;const selGames=gameDD.getSelected(),selPT=ptDD.getSelected(),selR=rDD.getSelected();const isOuting=currentView==="outing";const allRows=BY_PITCHER[player]||[];const rows=selGames===null?allRows:allRows.filter(r=>selGames.has(r[8]));const allTypes=getPitchTypes(player);const types=selPT===null?allTypes:allTypes.filter(t=>selPT.has(t));const datasets=types.map(pt=>{const pts=rows.filter(r=>r[1]===pt&&(selR===null||selR.has(rg(r[7],r[13]))));return{label:pt,data:pts.map(r=>({x:toIP(isOuting?r[10]:r[2]),y:r[5],raw:r})),backgroundColor:(COLORS[pt]||"#888")+"bb",borderColor:COLORS[pt]||"#888",borderWidth:.5,pointRadius:3.5,pointHoverRadius:6};});const allPts=datasets.flatMap(d=>d.data);if(allPts.length){const spins=allPts.map(p=>p.y),velos=allPts.map(p=>p.raw[6]);document.getElementById("cAvg").textContent=Math.round(spins.reduce((a,b)=>a+b)/spins.length).toLocaleString();document.getElementById("cMax").textContent=Math.max(...spins).toLocaleString();document.getElementById("cCnt").textContent=allPts.length.toLocaleString();document.getElementById("cVelo").textContent=(velos.reduce((a,b)=>a+b)/velos.length).toFixed(1);}else{["cAvg","cMax","cCnt","cVelo"].forEach(id=>document.getElementById(id).textContent="\u2014");}const leg=document.getElementById("leg");leg.innerHTML="";types.forEach(pt=>{const cnt=rows.filter(r=>r[1]===pt).length;const div=document.createElement("div");div.className="li";const dot=document.createElement("div");dot.className="dot";dot.style.background=COLORS[pt]||"#888";const lbl=document.createElement("span");lbl.textContent=pt;const c=document.createElement("span");c.style.cssText="color:#898781;font-size:11px";c.textContent=" ("+cnt+")";div.append(dot,lbl,c);leg.appendChild(div);});if(chart)chart.destroy();const allPX=rows.map(r=>toIP(isOuting?r[10]:r[2]));const pMax=allPX.length?Math.max(...allPX):8,pMin=allPX.length?Math.min(...allPX):0;const fullXMax=Math.ceil(pMax)+0.5,fullXMin=isOuting?0:Math.floor(pMin);let xMin=fullXMin,xMax=fullXMax;if(zoomedInning!==null){xMin=Math.max(fullXMin,(zoomedInning-1)-(1/6)-0.05);xMax=Math.min(fullXMax,zoomedInning-(1/6)+0.05);}chart=new Chart(document.getElementById("sc").getContext("2d"),{type:"scatter",data:{datasets},plugins:[customAxisPlugin],options:{responsive:true,maintainAspectRatio:false,animation:{duration:150},layout:{padding:{top:24,right:14,bottom:10}},scales:{x:{type:"linear",min:xMin-0.15,max:xMax,title:{display:true,text:isOuting?"Outs (outing progression)":"Outs",color:"#898781",font:{size:12},padding:{top:18}},ticks:{display:false},grid:{color:"rgba(0,0,0,.04)"}},y:{title:{display:true,text:"Spin rate (RPM)",color:"#898781",font:{size:12}},ticks:{color:"#898781",font:{size:11},callback:v=>v.toLocaleString()},grid:{color:"rgba(0,0,0,.06)"},min:600,max:3600}},plugins:{legend:{display:false},tooltip:{enabled:false}},onHover(e,els){const tt=document.getElementById("tt");if(!els.length){tt.style.opacity=0;return;}const r=datasets[els[0].datasetIndex].data[els[0].index].raw;document.getElementById("ttn").textContent=r[1];document.getElementById("ttb").innerHTML="Spin: <b>"+r[5].toLocaleString()+" RPM</b><br>"+"Velo: "+r[6].toFixed(1)+" MPH<br>"+"Game IP: "+fmtIP(toIP(r[2]))+(isOuting?"<br>Outing IP: "+fmtIP(toIP(r[10])):"")+  "<br>Result: "+(r[13]&&r[13]!==""?r[13].replace(/_/g," "):r[7].replace(/_/g," "))+"<br>"+fmtGameDate(r[8])+" vs "+r[11]+" ("+r[12]+")";tt.style.left=(e.native.clientX+14)+"px";tt.style.top=(e.native.clientY-20)+"px";tt.style.opacity=1;}}});const numInnings=Math.ceil(xMax-fullXMin);const minCW=Math.max(600,numInnings*120);const canvas=document.getElementById("sc");canvas.style.minWidth=Math.max(minCW,canvas.parentElement.clientWidth)+"px";chart.resize();if(!canvas.dataset.listenersAttached){canvas.dataset.listenersAttached="true";canvas.style.cursor="pointer";canvas.addEventListener("mouseleave",()=>{document.getElementById("tt").style.opacity=0;});canvas.addEventListener("click",function(e){if(!chart)return;const rect=this.getBoundingClientRect();const mouseX=e.clientX-rect.left,mouseY=e.clientY-rect.top;const xS=chart.scales.x,yS=chart.scales.y;if(mouseY>yS.top)return;for(let inn=1;inn<=12;inn++){const lineX=inn-(1/6);if(lineX<xS.min||lineX>xS.max)continue;const zoneStart=(inn-1)-(1/6)<xS.min?xS.min:(inn-1)-(1/6);const startPx=xS.getPixelForValue(zoneStart),endPx=xS.getPixelForValue(lineX);if(mouseX>=startPx&&mouseX<=endPx){if(zoomedInning===inn){resetZoom();}else{setZoom(inn);}return;}}});}}', out)
  writeLines('const pitchers=Object.keys(BY_PITCHER).sort((a,b)=>(DISPLAY[a]||a).localeCompare(DISPLAY[b]||b));', out)
  writeLines('const pSel=document.getElementById("pSel");', out)
  writeLines('const defaultOpt=document.createElement("option");', out)
  writeLines('defaultOpt.value="";defaultOpt.textContent="Select Pitcher";', out)
  writeLines('defaultOpt.disabled=true;defaultOpt.selected=true;', out)
  writeLines('pSel.appendChild(defaultOpt);', out)
  writeLines('function getPitcherStatus(p){const id=PITCHER_ID_MAP[p];if(!id)return"former";const rs=window.ROSTER_STATUS;if(rs.active&&rs.active.includes(id))return"active";if(rs.injured&&rs.injured.includes(id))return"injured";if(rs.forty_man&&rs.forty_man.includes(id))return"40man";return"former";}', out)
  writeLines('function addDivider(label){const div=document.createElement("option");div.disabled=true;div.textContent="\u2500\u2500 "+label+" \u2500\u2500";div.style.cssText="font-weight:600;color:#52514e;background:#f1efe8;";pSel.appendChild(div);}', out)
  writeLines('const STATUS_BG={active:"#ffffff",injured:"#ffd6d6",["40man"]:"#d6deff",former:"#e8e8e8"};', out)
  writeLines('const STATUS_COLOR={active:"#0b0b0b",injured:"#7a1f1f",["40man"]:"#1a2d5a",former:"#4a4a4a"};', out)
  writeLines('const grouped={active:[],injured:[],["40man"]:[],former:[]};', out)
  writeLines('pitchers.forEach(p=>{const s=getPitcherStatus(p);grouped[s].push(p);});', out)
  writeLines('function addPitcherOpts(list){list.forEach(p=>{const s=getPitcherStatus(p);const o=document.createElement("option");o.value=p;o.textContent=DISPLAY[p]||p;o.style.background=STATUS_BG[s];o.style.color=STATUS_COLOR[s];pSel.appendChild(o);});}', out)
  writeLines('addPitcherOpts(grouped.active);', out)
  writeLines('if(grouped.injured.length){addDivider("Injured List");addPitcherOpts(grouped.injured);}', out)
  writeLines('if(grouped["40man"].length){addDivider("40-Man Roster");addPitcherOpts(grouped["40man"]);}', out)
  writeLines('if(grouped.former.length){addDivider("Former Cardinals");addPitcherOpts(grouped.former);}', out)
  writeLines('pSel.addEventListener("change",function(){if(!pSel.value)return;zoomedInning=null;document.getElementById("zoomBar").style.display="none";initDropdowns(pSel.value);render();});', out)
  writeLines('rDD=buildDropdown("rList","All results",RESULT_OPTIONS,function(sel){updateLabel("rLabel",sel,"All results",function(v){const found=RESULT_OPTIONS.find(o=>o.value===v);return found?found.label:v;});render();});', out)
  writeLines('gameDD={getSelected:()=>null,reset:()=>{}};', out)
  writeLines('ptDD={getSelected:()=>null,reset:()=>{}};', out)
  writeLines('document.getElementById("cdBtn").style.pointerEvents="none";', out)
  writeLines('document.getElementById("cdBtn").style.opacity="0.45";', out)
  writeLines('document.getElementById("ptBtn").style.pointerEvents="none";', out)
  writeLines('document.getElementById("ptBtn").style.opacity="0.45";', out)
  writeLines('</script>', out)
  writeLines('</body>', out)
  writeLines('</html>', out)

  close(out)
  message(sprintf("Dashboard written: %s (%.0f KB)", output_html, file.size(output_html) / 1024))
  invisible(output_html)
}
