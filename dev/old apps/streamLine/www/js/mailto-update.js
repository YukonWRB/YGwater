// Custom message handler for updating the mailto link
Shiny.addCustomMessageHandler("updateMailtoLink", function(message) {
  document.getElementById("feedback_btn").href = message;
});
