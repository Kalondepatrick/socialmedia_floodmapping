document.addEventListener('DOMContentLoaded', function () {
    var map = L.map('map').setView([-12.5099647, 34.0985303], 15); 

    // Add the base map layer (ESRI Imagery)
    L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
        attribution: '© Esri',
        maxZoom: 19
    }).addTo(map);

    var marker = L.marker([-12.515362, 34.114150]).addTo(map); 
    marker.bindPopup("<b>Recognize this place!</b><br>This is Dwangwa bridge.").openPopup(); 
});
