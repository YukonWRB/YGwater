// When Shiny is fully connected, initialize window dimensions
$(document).on('shiny:connected', function(e) {
  Shiny.onInputChange('window_dimensions', {
    width:  $(window).width(),
    height: $(window).height()
  });
});

// On every resize event, update the dimensions
$(window).resize(function(e) {
  Shiny.onInputChange('window_dimensions', {
    width:  $(window).width(),
    height: $(window).height()
  });
});
