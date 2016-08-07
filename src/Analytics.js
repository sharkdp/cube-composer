exports.analyticsEvent = function(category) {
  return function(action) {
    return function(label) {
      return function() {
        ga('send', 'event', category, action, label);
        return {};
      };
    };
  };
};
