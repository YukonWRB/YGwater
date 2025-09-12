function initSidebarResize(opts) {
  const MIN_LEFT = 140;
  const MIN_RIGHT = 160;
  const MIN_MAIN = 300; // keep central workspace usable
  let resizing = '';
  let startX = 0;
  let startWidth = 0;
  const $body = $('body');
  const $left = $('#' + opts.leftId);
  const $right = $('#' + opts.rightId);

  function viewportW(){ return $(window).width(); }
  function maxLeft(){ return Math.max(MIN_LEFT, viewportW() - $right.outerWidth() - MIN_MAIN); }
  function maxRight(){ return Math.max(MIN_RIGHT, viewportW() - $left.outerWidth() - MIN_MAIN); }

  function startResize(side, e){
    resizing = side;
    startX = e.clientX;
    startWidth = (side === 'left' ? $left.outerWidth() : $right.outerWidth());
    $body.addClass('resizing-col');
    e.preventDefault();
  }

  $('#' + opts.leftHandle).off('.rs').on('mousedown.rs', e => startResize('left', e));
  $('#' + opts.rightHandle).off('.rs').on('mousedown.rs', e => startResize('right', e));

  $('#' + opts.leftHandle).off('dblclick.rs').on('dblclick.rs', () => $left.css('width', '300px'));
  $('#' + opts.rightHandle).off('dblclick.rs').on('dblclick.rs', () => $right.css('width', '400px'));

  $(document).off('.rsMove').on('mousemove.rsMove', function(e){
    if(!resizing) return;
    if(resizing === 'left'){
      let w = startWidth + (e.clientX - startX);
      w = Math.min(maxLeft(), Math.max(MIN_LEFT, w));
      $left.css('width', w + 'px');
    } else if(resizing === 'right'){
      let w = startWidth - (e.clientX - startX);
      w = Math.min(maxRight(), Math.max(MIN_RIGHT, w));
      $right.css('width', w + 'px');
    }
  });

  $(document).off('.rsUp').on('mouseup.rsUp', function(){
    if(resizing){
      resizing = '';
      $body.removeClass('resizing-col');
    }
  });

  $(window).off('.rsWin').on('resize.rsWin', function(){
    if($left.outerWidth() > maxLeft()) $left.css('width', maxLeft() + 'px');
    if($right.outerWidth() > maxRight()) $right.css('width', maxRight() + 'px');
  });

  const ids = opts.ids || [];
  ids.forEach(id => {
    $(document).off('focus.rs click.rs', '#' + id)
               .on('focus.rs click.rs', '#' + id, function(){
                 Shiny.setInputValue(id + '_clicked', Date.now(), {priority: 'event'});
               });
  });
}
