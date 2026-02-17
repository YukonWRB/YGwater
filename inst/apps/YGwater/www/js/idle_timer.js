(function() {
  var activityEvents = ["mousemove","mousedown","keydown","scroll","touchstart"];
  var throttleTimeout = null;
  var throttleDelayMs = 10000;

  function reportActivity() {
    if (throttleTimeout) return;
    throttleTimeout = setTimeout(function() { throttleTimeout = null; }, throttleDelayMs);

    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("user_last_activity", Date.now(), { priority: "event" });
    }
  }

  activityEvents.forEach(function(eventName) {
    document.addEventListener(eventName, reportActivity, { passive: true });
  });

  // Optional: improve scroll detection
  window.addEventListener("scroll", reportActivity, { passive: true });

  document.addEventListener("visibilitychange", reportActivity);

  // In Shiny, this is usually available:
  document.addEventListener("shiny:connected", reportActivity);

  // Initial ping
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", reportActivity);
  } else {
    reportActivity();
  }
})();
