$(document).ready(function() {
        window.showPage = function(pageId) {
          $('.page-section').removeClass('active');
          $('#' + pageId).addClass('active');
          if (pageId === 'home') {
            $('#homeNav').show();
          } else {
            $('#homeNav').hide();
          }
          window.scrollTo(0, 0);
        };
        window.showPage('home');


        window.flashPedigreeHighlight = function(ids, element, holdMs) {
          if (!Array.isArray(ids)) {
            ids = ids == null ? [] : [ids];
          }
          holdMs = holdMs || 900;
          if (window.pedigreeHighlightTimer) {
            clearTimeout(window.pedigreeHighlightTimer);
          }
          $('.ind-panel__family-chip, .ind-panel__family-label').removeClass('active');
          if (element) {
            $(element).addClass('active');
          }
          Shiny.setInputValue('summary_highlight_ids_direct', ids, {priority: 'event'});
          window.pedigreeHighlightTimer = setTimeout(function() {
            if (element) {
              $(element).removeClass('active');
            }
            Shiny.setInputValue('summary_highlight_ids_direct', null, {priority: 'event'});
          }, holdMs);
        };

        window.pedigreeCardsMove = function(direction) {
          var track = document.getElementById('pedigreeCardTrack');
          if (!track) return;
          var firstCard = track.querySelector('.pedigree-person-card');
          var step = firstCard ? firstCard.getBoundingClientRect().width + 12 : track.clientWidth * 0.85;
          track.scrollBy({left: direction * step, behavior: 'smooth'});
        };
      });

      $(document).on('contextmenu', '#pedPlot', function(e) {
        e.preventDefault();
        var offset = $(this).offset();
        var relX = e.pageX - offset.left;
        var relY = e.pageY - offset.top;
        Shiny.setInputValue('ped_context', {
          x: relX,
          y: relY,
          nonce: Math.random()
        }, {priority: 'event'});
      });

      $(document).on('click', function(e) {
        if (!$(e.target).closest('.context-menu').length) {
          Shiny.setInputValue('close_context_menu', Math.random());
        }
      });

      $(document).on('click', '.context-menu .has-submenu', function(e) {
        e.stopPropagation();
        $('.context-menu .has-submenu').not(this).removeClass('open');
        $(this).toggleClass('open');
      });

      $(document).on('click', '.context-menu .submenu, .context-menu .pos-btn', function(e) {
        e.stopPropagation();
      });

      $(document).on('click', '.context-menu .pos-btn', function() {
        $('.context-menu .has-submenu').removeClass('open');
      });

      $(document).on('mousedown', '.individual-float__header, .relationship-float__header', function(e) {
        var panel = $(this).closest('.individual-float, .relationship-float');
        var startX = e.clientX;
        var startY = e.clientY;
        var startLeft = panel.offset().left;
        var startTop = panel.offset().top;

        e.preventDefault();
        panel.addClass('is-dragging');

        $(document).on('mousemove.individualFloat', function(moveEvent) {
          var nextLeft = startLeft + moveEvent.clientX - startX;
          var nextTop = startTop + moveEvent.clientY - startY;
          var maxLeft = Math.max(8, window.innerWidth - panel.outerWidth() - 8);
          var maxTop = Math.max(8, window.innerHeight - panel.outerHeight() - 8);
          panel.css({
            left: Math.min(Math.max(8, nextLeft), maxLeft) + 'px',
            top: Math.min(Math.max(8, nextTop), maxTop) + 'px'
          });
        });

        $(document).on('mouseup.individualFloat', function() {
          panel.removeClass('is-dragging');
          $(document).off('.individualFloat');
        });
      });