(function() {
  var DOWNLOAD_INPUT_ID = "usage_download_click";
  var INPUT_CHANGED_ID = "usage_input_changed";
  var PER_INPUT_THROTTLE_MS = 1500;
  var GLOBAL_THROTTLE_MS = 150;

  var ignoredExact = {
    navbar: true,
    user_last_activity: true,
    userLang: true,
    window_dimensions: true,
    usage_download_click: true,
    usage_input_changed: true
  };

  var ignoredPatterns = [
    /^\.clientValue/,
    /^plotly_/,
    /^_$/,
    /^shinyjs-/,
    /_rows_(all|current)$/,
    /_rows_selected$/,
    /_search$/,
    /_state$/,
    /^last_viz_tab$/,
    /^last_admin_tab$/
  ];

  function canSend() {
    return window.Shiny && typeof Shiny.setInputValue === "function";
  }

  function shouldIgnoreInput(inputId) {
    if (!inputId || ignoredExact[inputId]) {
      return true;
    }
    for (var i = 0; i < ignoredPatterns.length; i++) {
      if (ignoredPatterns[i].test(inputId)) {
        return true;
      }
    }
    return false;
  }

  function emit(inputId, payload) {
    if (!canSend()) {
      return;
    }
    Shiny.setInputValue(inputId, payload, { priority: "event" });
  }

  document.addEventListener(
    "click",
    function(evt) {
      var target = evt.target;
      if (!target || typeof target.closest !== "function") {
        return;
      }

      var link = target.closest("a.shiny-download-link");
      if (!link) {
        return;
      }

      emit(DOWNLOAD_INPUT_ID, {
        input_id: link.id || null,
        filename_hint: link.getAttribute("download") || null,
        href: link.getAttribute("href") || null,
        location: window.location.pathname + window.location.search,
        ts: Date.now()
      });
    },
    true
  );

  var lastGlobalTs = 0;
  var lastInputTs = Object.create(null);

  document.addEventListener("shiny:inputchanged", function(evt) {
    if (!evt || !evt.detail) {
      return;
    }

    var inputId = evt.detail.name;
    if (shouldIgnoreInput(inputId)) {
      return;
    }

    var now = Date.now();
    if (now - lastGlobalTs < GLOBAL_THROTTLE_MS) {
      return;
    }

    var lastTs = lastInputTs[inputId] || 0;
    if (now - lastTs < PER_INPUT_THROTTLE_MS) {
      return;
    }

    lastGlobalTs = now;
    lastInputTs[inputId] = now;

    emit(INPUT_CHANGED_ID, {
      input_id: inputId,
      ts: now
    });
  });
})();
