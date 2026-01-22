(function() {
    var activityEvents = [
      "mousemove",
      "mousedown",
      "keydown",
      "scroll",
      "touchstart"
    ];
    var throttleTimeout = null;
    var throttleDelayMs = 10000;
  
    function reportActivity() {
      if (throttleTimeout) {
        return;
      }
      throttleTimeout = setTimeout(function() {
        throttleTimeout = null;
      }, throttleDelayMs);
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue("user_last_activity", Date.now(), {
          priority: "event"
        });
      }
    }
  
    activityEvents.forEach(function(eventName) {
      document.addEventListener(eventName, reportActivity, true);
    });
  
    document.addEventListener("visibilitychange", reportActivity);
  
    if (window.jQuery) {
      $(document).on("shiny:connected", reportActivity);
    } else {
      document.addEventListener("DOMContentLoaded", reportActivity);
    }
  })();
  