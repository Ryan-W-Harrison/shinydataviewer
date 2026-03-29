(function() {
  function syncSidebarHeight(root) {
    var tableRegion = root.querySelector(".de-table-card, .de-table-region");
    var sidebar = root.querySelector(".de-sidebar");

    if (!tableRegion || !sidebar) {
      return;
    }

    var tableHeight = tableRegion.getBoundingClientRect().height;

    if (tableHeight > 0) {
      root.style.setProperty("--de-panel-height", Math.round(tableHeight) + "px");
    }
  }

  function moveReactableControls(root) {
    var position = root.classList.contains("de-root--controls-bottom") ? "bottom" : "top";

    root.querySelectorAll(".de-table-card .reactable, .de-table-region .reactable").forEach(function(widget) {
      var search = widget.querySelector(".rt-search");
      var pagination = widget.querySelector(".rt-pagination");
      var table = widget.querySelector(".rt-table");

      if (!pagination || !table) {
        return;
      }

      if (position === "top") {
        var target = search ? search.nextSibling : table;

        if (target !== pagination) {
          widget.insertBefore(pagination, target);
        }
      } else if (table.nextSibling !== pagination) {
        widget.insertBefore(pagination, table.nextSibling);
      }
    });
  }

  function initializeReactableControls() {
    document.querySelectorAll(".de-root").forEach(function(root) {
      moveReactableControls(root);
      syncSidebarHeight(root);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeReactableControls);
  } else {
    initializeReactableControls();
  }

  var observer = new MutationObserver(function() {
    initializeReactableControls();
  });

  observer.observe(document.documentElement, {
    childList: true,
    subtree: true
  });

  if (typeof ResizeObserver !== "undefined") {
    var resizeObserver = new ResizeObserver(function(entries) {
      entries.forEach(function(entry) {
        var root = entry.target.closest(".de-root");

        if (root) {
          syncSidebarHeight(root);
        }
      });
    });

    document.querySelectorAll(".de-root .de-table-card, .de-root .de-table-region").forEach(function(node) {
      resizeObserver.observe(node);
    });

    var attachObserver = new MutationObserver(function() {
      document.querySelectorAll(".de-root .de-table-card, .de-root .de-table-region").forEach(function(node) {
        resizeObserver.observe(node);
      });
    });

    attachObserver.observe(document.documentElement, {
      childList: true,
      subtree: true
    });
  }
})();
