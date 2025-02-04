// store a resize timeout ID so we can debounce events
var resizeTimeout;

// 1) When Shiny is connected, send initial window dimensions
$(document).on('shiny:connected', function(e) {
  sendWindowSizeToShiny();
});

// 2) On window resize, debounce and then send updated dimensions
$(window).on("resize", function(e) {
  clearTimeout(resizeTimeout);
  resizeTimeout = setTimeout(function() {
    sendWindowSizeToShiny();
  }, 500); // 500ms debounce
});

// 3) On mobile orientation change, do a slightly longer delay
//    to ensure rotation finishes and the layout stabilizes
window.addEventListener("orientationchange", function() {
  clearTimeout(resizeTimeout);
  resizeTimeout = setTimeout(function() {
    sendWindowSizeToShiny();
  }, 700); // 700ms for rotation
});

// ---------------------------------------------------------------------------
// The function that actually sends window width/height to Shiny
function sendWindowSizeToShiny() {
  Shiny.onInputChange('window_dimensions', {
    width:  $(window).width(),
    height: $(window).height(),
    // Adding a timestamp ensures Shiny sees a "new" value,
    // even if the width/height didn't change between calls
    timestamp: new Date().getTime()
  });
}

// ---------------------------------------------------------------------------
// 4) A reusable function to track any container's size rather than the whole window
//    Call trackContainerSize(...) from Shiny (e.g. via shinyjs::runjs())
//    to monitor changes in that specific container.
function trackContainerSize(containerId, shinyInputId) {
  
  // We'll define an inner function that sends the container's bounding box to Shiny
  function sendContainerSize() {
    var container = document.getElementById(containerId);
    if (!container) return;
    var rect = container.getBoundingClientRect();
    Shiny.setInputValue(shinyInputId, {
      width:  rect.width,
      height: rect.height,
      timestamp: new Date().getTime()
    });
  }

  // Send the size once on document ready
  $(document).ready(function() {
    sendContainerSize();

    // Listen for window resize (which affects the container size)
    $(window).on("resize", function() {
      clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(function() {
        sendContainerSize();
      }, 500);
    });
    
    // Also watch for orientation changes (optional)
    window.addEventListener("orientationchange", function() {
      clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(function() {
        sendContainerSize();
      }, 700);
    });
  });
}
