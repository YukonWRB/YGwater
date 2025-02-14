// www/js.check_speed.js

// This function does the actual speed test
function checkSpeed() {
  console.log("checkSpeed: Starting speed check...");
  var startTime = (new Date()).getTime();
  // A tiny 1x1 GIF from Google (~43 bytes)
  var imageUrl = "https://www.google.com/images/phd/px.gif?t=" + startTime;

  var img = new Image();
  img.onload = function() {
    var endTime = (new Date()).getTime();
    var durationSec = (endTime - startTime) / 1000;
    var fileSizeKB = 0.043; // size in KB
    // Convert to Mbps
    var speedMbps = (fileSizeKB / durationSec) * 8 / 1024;

    console.log("checkSpeed: Measured speed =", speedMbps, "Mbps");
    // Send the speed to Shiny (input$user_speed)
    Shiny.setInputValue("user_speed", speedMbps, {priority: "event"});
  };
  img.onerror = function() {
    console.log("checkSpeed: Image load failed, defaulting speed to 0.5 Mbps");
    Shiny.setInputValue("user_speed", 0.5, {priority: "event"});
  };
  img.src = imageUrl;
}

// 1) Define a custom message handler that Shiny calls
Shiny.addCustomMessageHandler("checkSpeed", function(msg) {
  checkSpeed();
});
