exports.installSortable = function (el) {
  return function(sortHandler) {
    return function() {
      new Sortable(el, {
        group: 'function-lists',
        ghostClass: 'sortable-ghost',
        animation: 150,
        onSort: sortHandler
      });
    };
  };
};
