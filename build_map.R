library(leaflet)
library(htmlwidgets)
library(htmltools)
library(base64enc)
library(geojsonsf)
library(sf)

# --- STEP 1: LOAD & PREPARE DATA ---
if (!file.exists("in_boundary.json")) stop("Error: 'in_boundary.json' missing.")

# 1. Load Boundary
in_simple <- read_sf("in_boundary.json")
india_json_str <- sf_geojson(in_simple)

# 2. Load Icon
encode_svg <- function(path) {
  if (!file.exists(path)) stop(paste("Missing file at:", path))
  paste0("data:image/svg+xml;base64,", base64encode(path))
}
marker_uri <- encode_svg("www/icons/Map_marker.svg")

# 3. Bundle Data
map_data_bundle <- list(
  marker = marker_uri
)

# --- STEP 2: JAVASCRIPT LOGIC ---
js_logic <- "
  function(el, x, bundledData) {
    var map = this;
    var markerIcon = bundledData.marker;

    // --- HELPERS: popup content ---
    function esc(s){
      if (s === null || s === undefined) return '';
      return String(s)
        .replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;')
        .replace(/\"/g,'&quot;');
    }
    function isUrl(s){
      if (s === null || s === undefined) return false;
      var t = String(s).trim().toLowerCase();
      return t.indexOf('http://') === 0 || t.indexOf('https://') === 0;
    }
    function waLink(phone){
      if (phone === null || phone === undefined || phone === '') return null;
      var d = String(phone).replace(/[^0-9]/g, '');
      if (d.length === 10) d = '91' + d;          // add India country code
      if (d.length < 11 || d.length > 13) return null;
      return 'https://wa.me/' + d;
    }
    function coordRow(name, email, phone){
      if (!name && !email && !phone) return '';
      var links = '';
      if (email) links += '<a class=\"cbc-chip\" href=\"mailto:'+esc(email)+'\" title=\"Email\">&#9993;</a>';
      var wa = waLink(phone);
      if (wa) links += '<a class=\"cbc-chip cbc-wa\" href=\"'+wa+'\" target=\"_blank\" rel=\"noopener\" title=\"WhatsApp\">WhatsApp</a>';
      return '<div class=\"cbc-coord\">' +
               '<span class=\"cbc-coord-name\">'+esc(name || 'Coordinator')+'</span>' +
               '<span class=\"cbc-coord-links\">'+links+'</span>' +
             '</div>';
    }
    function buildPopup(p){
      var html = '<div class=\"cbc-hover-card\">';

      // Title (linked to website if present)
      var title = esc(p.campus);
      if (isUrl(p.web)) title = '<a href=\"'+esc(p.web)+'\" target=\"_blank\" rel=\"noopener\">'+title+'</a>';
      html += '<div class=\"cbc-title\">'+title+'</div>';

      // Subtitle: State . Area
      var subParts = [];
      if (p.state) subParts.push(esc(p.state));
      if (p.area)  subParts.push(esc(p.area));
      if (subParts.length) html += '<div class=\"cbc-sub\">'+subParts.join(' &middot; ')+'</div>';

      // Coordinators
      var coords = coordRow(p.coordinator1, p.email1, p.phone1) +
                   coordRow(p.coordinator2, p.email2, p.phone2);
      if (coords) html += '<div class=\"cbc-divider\"></div><div class=\"cbc-coord-wrap\">'+coords+'</div>';

      // Action pills (only real URLs)
      var pills = '';
      if (isUrl(p.gmap))  pills += '<a class=\"cbc-pill\" href=\"'+esc(p.gmap)+'\" target=\"_blank\" rel=\"noopener\">Directions</a>';
      if (isUrl(p.web))   pills += '<a class=\"cbc-pill\" href=\"'+esc(p.web)+'\" target=\"_blank\" rel=\"noopener\">Website</a>';
      if (isUrl(p.ebird)) pills += '<a class=\"cbc-pill\" href=\"'+esc(p.ebird)+'\" target=\"_blank\" rel=\"noopener\">eBird</a>';
      if (isUrl(p.inat))  pills += '<a class=\"cbc-pill\" href=\"'+esc(p.inat)+'\" target=\"_blank\" rel=\"noopener\">iNaturalist</a>';
      if (pills) html += '<div class=\"cbc-divider\"></div><div class=\"cbc-pills\">'+pills+'</div>';

      html += '</div>';
      return html;
    }

    // 1. ADD CONTROLS
    L.control.zoom({ position: 'topright' }).addTo(map);
    
    var recenter = L.control({position: 'topright'});
    recenter.onAdd = function(map) {
      var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
      div.innerHTML = '<a href=\"#\" title=\"Reset View\" style=\"background-color: white; width: 30px; height: 30px; line-height: 30px; text-align: center; display: block; cursor: pointer; color: black; font-size: 22px; font-weight: bold; text-decoration: none;\">&#8962;</a>';
      div.onclick = function(e) {
        L.DomEvent.stopPropagation(e);
        L.DomEvent.preventDefault(e);
        map.setView([22.5937, 78.9629], 5);
      };
      return div;
    };
    recenter.addTo(map);

    var dashboard = L.control({position: 'topleft'});
    dashboard.onAdd = function (map) {
      var div = L.DomUtil.create('div', 'stats-dashboard');
      div.innerHTML = 
        '<div class=\"stat-item\"><div class=\"stat-value\" id=\"dash-campuses\">--</div><div class=\"stat-label\">Campuses</div></div>' +
        '<div class=\"stat-item\"><div class=\"stat-value\" id=\"dash-states\">-- / 37</div><div class=\"stat-label\">States / UTs</div></div>';
      return div;
    };
    dashboard.addTo(map);

    // 2. FETCH DATA
    var baseUrl = 'https://raw.githubusercontent.com/birdcountindia/cbr-map/main/';
    fetch(baseUrl + 'no_of_campuses.txt').then(r => r.text()).then(t => { document.getElementById('dash-campuses').innerText = t.trim(); });
    fetch(baseUrl + 'no_of_states.txt').then(r => r.text()).then(t => { document.getElementById('dash-states').innerText = t.trim() + ' / 37'; });

    // --- TIMER LOGIC ---
    fetch(baseUrl + 'last_update.txt').then(r => r.text()).then(ts => {
        var lastUpdate = new Date(ts.trim());
        var timerElement = document.getElementById('update-timer');
        
        function updateCounter() {
          var now = new Date();
          var diffMs = now - lastUpdate;
          
          if (isNaN(diffMs)) { 
             if(timerElement) timerElement.innerHTML = 'Status: Online'; 
             return; 
          }
          
          var diffHrs = Math.floor(diffMs / 3600000);
          var diffMins = Math.floor((diffMs % 3600000) / 60000);

          if (timerElement) {
              if (diffHrs > 0) {
                 timerElement.innerHTML = \"This map was last updated \" + diffHrs + \" hours and \" + diffMins + \" minutes ago.\";
              } else {
                 timerElement.innerHTML = \"This map was last updated \" + diffMins + \" minutes ago.\";
              }
              
              if (diffHrs >= 2) { 
                timerElement.style.color = '#e74c3c'; 
                timerElement.style.fontWeight = 'bold';
              } else {
                timerElement.style.color = '#555'; 
                timerElement.style.fontWeight = 'normal';
              }
          }
        }
        updateCounter(); 
        setInterval(updateCounter, 60000);
    });

    // 3. CAMPUS MARKERS
    fetch(baseUrl + 'campuses.json').then(r => r.json()).then(data => {
        L.geoJson(data, {
          pointToLayer: function (feature, latlng) {
            var isMobile = window.innerWidth < 600;
            var iconSize = isMobile ? [35, 35] : [25, 25];
            return L.marker(latlng, { 
              icon: L.icon({ 
                iconUrl: markerIcon, 
                iconSize: iconSize, 
                iconAnchor: [iconSize[0]/2, iconSize[1]] 
              }),
              group: 'Campuses' 
            });
          },
          onEachFeature: function (f, l) {
            l.bindPopup(buildPopup(f.properties), {
              className: 'cbc-hover-popup',
              closeButton: true,
              autoClose: true,
              closeOnClick: true,
              maxWidth: 320,
              minWidth: 240,
              autoPan: true
            });
            // Flashy hover on desktop; tap still opens on mobile (no mouseover there).
            // We deliberately do NOT close on mouseout so the links stay clickable.
            l.on('mouseover', function () { this.openPopup(); });
          }
        }).addTo(map);
    });
  }
"

# --- STEP 3: BUILD MAP ---
map_shell <- leaflet(options = leafletOptions(
  minZoom = 4, maxZoom = 15, zoomControl = FALSE, dragging = TRUE, tap = TRUE, touchZoom = TRUE
)) %>%
  setView(lng = 78.9629, lat = 22.5937, zoom = 5) %>%
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}&apistyle=s.t:2|s.e:g|p.v:off") %>%
  addPolygons(data = in_simple, fill = FALSE, color = "#333333", weight = 1.5, opacity = 1.0, options = pathOptions(interactive = FALSE)) %>%
  prependContent(tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    
    # --- WHATSAPP / SOCIAL MEDIA PREVIEW TAGS ---
    tags$meta(property="og:title", content="Campus Biodiversity Register"),
    tags$meta(property="og:description", content="Explore registered campuses and events for the Campus Biodiversity Register."),
    tags$meta(property="og:url", content="https://birdcountindia.github.io/cbr-map/"),
    tags$meta(property="og:type", content="website"),
    
    tags$style(HTML("
      body, html, #htmlwidget_container, .leaflet { width: 100%; height: 100%; margin: 0; padding: 0; overflow: hidden; }

      /* ---------- FLASHY-BUT-MINIMAL HOVER CARD ---------- */
      /* Strip Leaflet's default bubble so the card floats cleanly */
      .cbc-hover-popup .leaflet-popup-content-wrapper { background: transparent; box-shadow: none; padding: 0; border-radius: 18px; }
      .cbc-hover-popup .leaflet-popup-content { margin: 0; }
      .cbc-hover-popup .leaflet-popup-tip-container { display: none; }
      .cbc-hover-popup .leaflet-popup-close-button { color: #b0b8c0; z-index: 5; padding: 6px 8px 0 0; }
      .cbc-hover-popup .leaflet-popup-close-button:hover { color: #e74c3c; }

      .cbc-hover-card {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        min-width: 220px; max-width: 300px;
        background: #ffffff;
        border-radius: 18px;
        padding: 16px 18px;
        border: 1px solid rgba(0,0,0,0.06);
        box-shadow: 0 12px 34px rgba(0,0,0,0.20);
        white-space: normal;
      }
      .cbc-title { text-align: center; font-size: 18px; font-weight: 800; text-transform: capitalize; color: #1f2d3d; margin: 0 0 2px; line-height: 1.25; word-wrap: break-word; }
      .cbc-title a { color: #1f2d3d; text-decoration: none; }
      .cbc-title a:hover { color: #e74c3c; }
      .cbc-sub { text-align: center; font-size: 12px; color: #7a8794; text-transform: capitalize; margin-bottom: 2px; }
      .cbc-divider { height: 1px; background: linear-gradient(to right, transparent, #e6e9ec, transparent); margin: 12px 0; }
      .cbc-coord-wrap { display: flex; flex-direction: column; gap: 10px; }
      .cbc-coord { display: flex; align-items: center; justify-content: space-between; gap: 8px; }
      .cbc-coord-name { font-size: 13px; font-weight: 600; color: #2c3e50; }
      .cbc-coord-links { display: flex; gap: 6px; flex-shrink: 0; }
      .cbc-chip { display: inline-flex; align-items: center; justify-content: center; height: 28px; min-width: 28px; padding: 0 9px; border-radius: 9px; font-size: 13px; font-weight: 700; text-decoration: none; background: #f1f3f5; color: #495057; transition: transform .12s ease, background .12s ease; }
      .cbc-chip:hover { transform: translateY(-1px); }
      .cbc-wa { background: #25D366; color: #fff; }
      .cbc-wa:hover { background: #1da851; }
      .cbc-pills { display: flex; flex-wrap: wrap; gap: 8px; justify-content: center; }
      .cbc-pill { flex: 1 1 auto; text-align: center; padding: 9px 14px; border-radius: 10px; font-size: 12px; font-weight: 700; text-decoration: none; background: #fff; color: #e74c3c; border: 1.5px solid #e74c3c; transition: all .12s ease; white-space: nowrap; }
      .cbc-pill:hover { background: #e74c3c; color: #fff; }
      /* --------------------------------------------------- */

      #update-timer { position: absolute; bottom: 12px; left: 12px; z-index: 1000; background: rgba(255, 255, 255, 0.95); padding: 8px 12px; border-radius: 8px; font-family: Helvetica, sans-serif; font-weight: bold; font-size: 11px; border: 1px solid #ddd; max-width: 180px; text-align: center; line-height: 1.4; box-shadow: 0 4px 10px rgba(0,0,0,0.15); color: #333; }
      #bci-logo { position: absolute; bottom: 12px; right: 12px; z-index: 1000; }
      #bci-logo img { height: 65px; width: auto; opacity: 1.0; border-radius: 12px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
      .stats-dashboard { background: rgba(255, 255, 255, 0.95); padding: 8px 12px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.2); font-family: 'Helvetica Neue', Arial, sans-serif; min-width: 110px; margin-top: 10px !important; }
      .stat-item { margin-bottom: 8px; text-align: center; border-bottom: 1px solid #eee; padding-bottom: 4px; }
      .stat-item:last-child { border-bottom: none; margin-bottom: 0; }
      .stat-label { font-size: 10px; text-transform: uppercase; letter-spacing: 0.5px; color: #666; margin-top: -2px; }
      .stat-value { font-size: 26px; font-weight: 800; color: #e74c3c; line-height: 1.0; }
      @media (max-width: 600px) { #update-timer { font-size: 9px; bottom: 10px; left: 10px; max-width: 140px; padding: 6px; } #bci-logo img { height: 48px; } #bci-logo { bottom: 10px; right: 10px; } .stats-dashboard { transform: scale(0.8); transform-origin: top left; } .cbc-hover-card { min-width: 200px; max-width: 280px; } }
    "))
  )) %>%
  appendContent(tags$div(id = "bci-logo", tags$img(src = "icons/logos.png")),
                tags$div(id = "update-timer", "Calculating last update...")) %>%
  onRender(js_logic, data = map_data_bundle)

# --- STEP 4: SAVE ---
saveWidget(map_shell, file = "CBR_map.html", selfcontained = TRUE)