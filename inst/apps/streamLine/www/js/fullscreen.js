function toggleFullScreen(elementId) {
  var elem = document.getElementById(elementId);
  if (elem) {
  if (!document.fullscreenElement) {
    elem.requestFullscreen().catch(err => {
      alert("Error attempting to enable full-screen mode: " + err.message);
    });
  } else {
    document.exitFullscreen();
  }
  } else {
    console.error("Element with ID '" + elementId + "' not found.");
  }
}
