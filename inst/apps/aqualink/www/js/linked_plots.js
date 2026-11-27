// Link the x-axes of two plotly plots: zoom, pan and wheel zoom on one are
// applied to the other, double-click (or "reset axes") or redrawing either
// plot returns both to the plotted date window, and the hover line is
// mirrored onto the other plot.
// Used by the image timeseries module (imgTsView). Each plot calls
// linkedPlotsSyncX from its htmlwidgets onRender with
// {partner: "<other plot id>", key: "<window key>", range: ["<start>", "<end>"]}.
// Plots only follow each other while they were drawn for the same window key.
(function () {
  function setRange(gd, range) {
    gd._syncing = true;
    return Plotly.relayout(gd, { "xaxis.range": range.slice() }).then(
      function () { gd._syncing = false; },
      function () { gd._syncing = false; }
    );
  }

  function partnerOf(el) {
    var other = document.getElementById(el._syncWindow.partner);
    if (other && other.layout && other._syncWindow &&
        other._syncWindow.key === el._syncWindow.key) {
      return other;
    }
    return null;
  }

  function onRelayout(el, ev) {
    if (el._syncing) {
      return;
    }
    var range = null;
    var reset = false;
    if (ev["xaxis.autorange"]) {
      // Double-click or "reset axes": go back to the selected date window
      // rather than each plot's own data extent
      range = el._syncWindow.range;
      reset = true;
    } else if (ev["xaxis.range[0]"] !== undefined) {
      range = [ev["xaxis.range[0]"], ev["xaxis.range[1]"]];
    } else if (ev["xaxis.range"]) {
      range = ev["xaxis.range"];
    }
    if (!range) {
      return;
    }
    if (reset) {
      setRange(el, range);
    }
    var other = partnerOf(el);
    if (other) {
      setRange(other, range);
    }
  }

  // Hover line mirrored onto the partner plot (null clears it). Drawn as a
  // layout shape; batched per animation frame to keep hovering smooth.
  function mirrorHover(el, x) {
    var other = partnerOf(el);
    if (!other) {
      return;
    }
    other._pendingHoverX = x;
    if (other._hoverFrame) {
      return;
    }
    other._hoverFrame = window.requestAnimationFrame(function () {
      other._hoverFrame = null;
      var hx = other._pendingHoverX;
      Plotly.relayout(other, {
        shapes: hx === null ? [] : [{
          type: "line",
          xref: "x",
          yref: "paper",
          x0: hx,
          x1: hx,
          y0: 0,
          y1: 1,
          line: { color: "#444", width: 1, dash: "dot" }
        }]
      });
    });
  }

  window.linkedPlotsSyncX = function (el, syncWindow) {
    el._syncWindow = syncWindow;

    // Plotly drops listeners when it redraws, so re-attach on every render
    if (el._linkedHandlers && el.removeListener) {
      Object.keys(el._linkedHandlers).forEach(function (evt) {
        el.removeListener(evt, el._linkedHandlers[evt]);
      });
    }
    el._linkedHandlers = {
      plotly_relayout: function (ev) { onRelayout(el, ev); },
      plotly_hover: function (ev) {
        if (ev && ev.points && ev.points.length) {
          mirrorHover(el, ev.points[0].x);
        }
      },
      plotly_unhover: function () { mirrorHover(el, null); }
    };
    Object.keys(el._linkedHandlers).forEach(function (evt) {
      el.on(evt, el._linkedHandlers[evt]);
    });

    // A redrawn plot shows the whole date window, so put a zoomed partner
    // back on the window too; otherwise both could sit on an old zoom that no
    // longer matches the date range input
    var other = partnerOf(el);
    if (other) {
      setRange(other, syncWindow.range);
    }
  };
})();
