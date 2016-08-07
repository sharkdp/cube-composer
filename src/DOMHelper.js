exports.getSelectedValue = function (src) {
    return function() {
        return src.options[src.selectedIndex].value;
    };
};

exports.setInnerHTML = function (value) {
  return function (element) {
    return function () {
      element.innerHTML = value;
      return {};
    };
  };
};

exports.htmlCollectionToArray = function (collection) {
  return Array.prototype.slice.call(collection);
};

exports.keyCode = function (ev) {
  return ev.keyCode;
};

exports.ctrlKey = function (ev) {
  return ev.ctrlKey;
};

exports.setStyleAttribute = function (name) {
  return function (value) {
    return function (element) {
      return function () {
        element.style[name] = value;
        return {};
      };
    };
  };
};


exports.classAdd = function (value) {
  return function (element) {
    return function () {
      element.classList.add(value);
      return {};
    };
  };
};

exports.classRemove = function (value) {
  return function (element) {
    return function () {
      element.classList.remove(value);
      return {};
    };
  };
};
