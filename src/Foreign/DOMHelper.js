// module DOMHelper

exports.parentElement = function (child) {
    return function() {
        return child.parentElement;
    };
};

exports.getLocationHash = function (loc) {
    return function() {
        if (loc.hash) {
            return loc.hash.substring(1);
        } else {
            return "";
        }
    };
};

exports.addChangeEventListener = function (cb) {
    return function(src) {
        return function() {
            src.addEventListener("change", cb(src));
        };
    };
};

exports.getSelectedValue = function (src) {
    return function() {
        return src.options[src.selectedIndex].value;
    };
};
