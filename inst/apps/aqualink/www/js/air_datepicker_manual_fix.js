(function() {
  var PATCH_FLAG = "__ygManualTextFixPatched";
  var FORMAT_CACHE = Object.create(null);
  var TIMEZONE_FORMATTER_CACHE = Object.create(null);
  var TOKEN_PATTERNS = {
    yyyy: "(\\d{4})",
    yy: "(\\d{2})",
    MM: "(\\d{2})",
    M: "(\\d{1,2})",
    dd: "(\\d{2})",
    d: "(\\d{1,2})",
    HH: "(\\d{2})",
    H: "(\\d{1,2})",
    hh: "(\\d{2})",
    h: "(\\d{1,2})",
    mm: "(\\d{2})",
    m: "(\\d{1,2})",
    ss: "(\\d{2})",
    s: "(\\d{1,2})",
    A: "([AP]M)",
    a: "([ap]m)",
    AA: "([AP]M)",
    aa: "([ap]m)"
  };
  var TOKEN_ORDER = Object.keys(TOKEN_PATTERNS).sort(function(a, b) {
    return b.length - a.length;
  });

  function escapeRegex(text) {
    return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  }

  function escapeSelectorValue(text) {
    if (window.CSS && typeof window.CSS.escape === "function") {
      return window.CSS.escape(text);
    }
    return String(text).replace(/["\\]/g, "\\$&");
  }

  function getBinding() {
    if (!window.Shiny || !Shiny.inputBindings || !Shiny.inputBindings.bindingNames) {
      return null;
    }
    var bindingObj = Shiny.inputBindings.bindingNames["shinyWidgets.AirDatepicker"];
    if (!bindingObj || !bindingObj.binding) {
      return null;
    }
    return bindingObj.binding;
  }

  function getFormatSpec(format) {
    if (FORMAT_CACHE[format]) {
      return FORMAT_CACHE[format];
    }

    var tokens = [];
    var pattern = "^\\s*";
    var i = 0;

    while (i < format.length) {
      var token = null;
      for (var j = 0; j < TOKEN_ORDER.length; j++) {
        var candidate = TOKEN_ORDER[j];
        if (format.slice(i, i + candidate.length) === candidate) {
          token = candidate;
          break;
        }
      }

      if (token) {
        tokens.push(token);
        pattern += TOKEN_PATTERNS[token];
        i += token.length;
      } else {
        var ch = format.charAt(i);
        pattern += /\s/.test(ch) ? "\\s*" : escapeRegex(ch);
        i += 1;
      }
    }

    pattern += "\\s*$";
    FORMAT_CACHE[format] = {
      regex: new RegExp(pattern),
      tokens: tokens
    };
    return FORMAT_CACHE[format];
  }

  function normalizeTwoDigitYear(year) {
    if (year >= 70) {
      return 1900 + year;
    }
    return 2000 + year;
  }

  function buildDate(parts) {
    var now = new Date();
    var year = parts.year != null ? parts.year : now.getFullYear();
    var month = parts.month != null ? parts.month : now.getMonth() + 1;
    var day = parts.day != null ? parts.day : now.getDate();
    var hour = parts.hour != null ? parts.hour : 0;
    var minute = parts.minute != null ? parts.minute : 0;
    var second = parts.second != null ? parts.second : 0;

    if (parts.ampm) {
      var ampm = parts.ampm.toLowerCase();
      if (hour === 12) {
        hour = 0;
      }
      if (ampm === "pm") {
        hour += 12;
      }
    }

    var date = new Date(year, month - 1, day, hour, minute, second, 0);
    if (
      date.getFullYear() !== year ||
      date.getMonth() !== month - 1 ||
      date.getDate() !== day ||
      date.getHours() !== hour ||
      date.getMinutes() !== minute ||
      date.getSeconds() !== second
    ) {
      return null;
    }

    return date;
  }

  function parseFormattedValue(text, format) {
    if (!format || typeof format !== "string") {
      return null;
    }

    var spec = getFormatSpec(format);
    var match = spec.regex.exec(text);
    if (!match) {
      return null;
    }

    var parts = {
      year: null,
      month: null,
      day: null,
      hour: null,
      minute: null,
      second: null,
      ampm: null
    };

    for (var i = 0; i < spec.tokens.length; i++) {
      var token = spec.tokens[i];
      var raw = match[i + 1];
      switch (token) {
        case "yyyy":
          parts.year = parseInt(raw, 10);
          break;
        case "yy":
          parts.year = normalizeTwoDigitYear(parseInt(raw, 10));
          break;
        case "MM":
        case "M":
          parts.month = parseInt(raw, 10);
          break;
        case "dd":
        case "d":
          parts.day = parseInt(raw, 10);
          break;
        case "HH":
        case "H":
        case "hh":
        case "h":
          parts.hour = parseInt(raw, 10);
          break;
        case "mm":
        case "m":
          parts.minute = parseInt(raw, 10);
          break;
        case "ss":
        case "s":
          parts.second = parseInt(raw, 10);
          break;
        case "AA":
        case "aa":
        case "A":
        case "a":
          parts.ampm = raw;
          break;
      }
    }

    return buildDate(parts);
  }

  function splitManualValues(text, instance) {
    var separator = instance && instance.opts
      ? instance.opts.multipleDatesSeparator
      : null;

    if (!separator || typeof separator !== "string") {
      return [text];
    }

    if (
      !(instance.opts && (instance.opts.range || instance.opts.multipleDates))
    ) {
      return [text];
    }

    return text.split(separator).map(function(value) {
      return value.trim();
    });
  }

  function replaceSelectedDates(instance, parsedDates) {
    if (
      !instance ||
      typeof instance.replaceDate !== "function" ||
      !Array.isArray(instance.selectedDates)
    ) {
      return false;
    }

    if (
      instance.opts &&
      (instance.opts.range || instance.opts.multipleDates)
    ) {
      if (instance.selectedDates.length !== parsedDates.length) {
        return false;
      }

      for (var i = 0; i < parsedDates.length; i++) {
        if (!(instance.selectedDates[i] instanceof Date)) {
          return false;
        }
      }

      for (var j = 0; j < parsedDates.length; j++) {
        instance.replaceDate(instance.selectedDates[j], parsedDates[j]);
      }
      return true;
    }

    if (instance.selectedDates.length !== 1 || !(instance.selectedDates[0] instanceof Date)) {
      return false;
    }

    instance.replaceDate(instance.selectedDates[0], parsedDates[0]);
    return true;
  }

  function cloneDates(dates) {
    return dates.map(function(date) {
      return new Date(date.getTime());
    });
  }

  function syncInstanceView(instance, parsedDates) {
    if (!instance || !parsedDates.length) {
      return;
    }

    var syncedDates = cloneDates(parsedDates);
    var activeDate = syncedDates[syncedDates.length - 1];

    if (Array.isArray(instance.selectedDates)) {
      instance.selectedDates = syncedDates;
    }
    instance.lastSelectedDate = activeDate;

    if (instance.opts && instance.opts.range) {
      instance.rangeDateFrom = syncedDates[0] || false;
      instance.rangeDateTo = syncedDates[1] || false;
    }

    if (typeof instance.setViewDate === "function") {
      instance.setViewDate(new Date(activeDate.getTime()));
    } else {
      instance.viewDate = new Date(activeDate.getTime());
      instance.date = new Date(activeDate.getTime());
    }

    if (typeof instance.setFocusDate === "function") {
      instance.setFocusDate(new Date(activeDate.getTime()), {
        viewDateTransition: false
      });
    } else {
      instance.focusDate = new Date(activeDate.getTime());
    }

    if (instance.nav && typeof instance.nav.render === "function") {
      instance.nav.render();
    }
    if (Array.isArray(instance.views)) {
      instance.views.forEach(function(view) {
        if (view && typeof view.render === "function") {
          view.render();
        }
      });
    }
  }

  function syncInstanceFromInput(binding, el) {
    if (!binding || !binding.store) {
      return "unavailable";
    }

    var instance = binding.store[el.id];
    if (!instance || !instance.elIsInput || typeof instance.selectDate !== "function") {
      return "unavailable";
    }

    var rawText = (el.value || "").trim();
    var widgetText = "";
    if (typeof instance._getInputValue === "function" && instance.locale) {
      widgetText = String(instance._getInputValue(instance.locale.dateFormat) || "").trim();
    }

    if (rawText === widgetText) {
      return "unchanged";
    }

    if (rawText === "") {
      if (typeof instance.clear === "function" && instance.selectedDates.length) {
        instance.clear({ silent: true });
        return "cleared";
      }
      return "unchanged";
    }

    var format = instance.locale && instance.locale.dateFormat;
    if (typeof format !== "string") {
      return "invalid";
    }

    var parts = splitManualValues(rawText, instance);
    var parsedDates = [];

    for (var i = 0; i < parts.length; i++) {
      var part = parts[i];
      if (!part) {
        return "invalid";
      }
      var parsed = parseFormattedValue(part, format);
      if (!(parsed instanceof Date) || isNaN(parsed.getTime())) {
        return "invalid";
      }
      parsedDates.push(parsed);
    }

    if (!replaceSelectedDates(instance, parsedDates)) {
      instance.selectDate(
        instance.opts && (instance.opts.range || instance.opts.multipleDates)
          ? parsedDates
          : parsedDates[0],
        { silent: true, updateTime: true }
      );
    }
    syncInstanceView(instance, parsedDates);
    return "synced";
  }

  function isAirDatepickerInput(el) {
    return Boolean(
      el &&
      el.nodeType === 1 &&
      el.classList &&
      el.classList.contains("sw-air-picker")
    );
  }

  function pushInputValue(binding, el) {
    if (
      !window.Shiny ||
      typeof Shiny.setInputValue !== "function" ||
      typeof binding.getValue !== "function"
    ) {
      return;
    }

    var inputName = el.id;
    if (typeof binding.getType === "function") {
      inputName += ":" + binding.getType(el);
    }

    Shiny.setInputValue(inputName, binding.getValue(el), {
      priority: "event"
    });
  }

  function handleManualSyncEvent(event) {
    var el = event && event.target;
    if (!isAirDatepickerInput(el)) {
      return;
    }

    var binding = getBinding();
    if (!binding) {
      return;
    }

    var syncStatus = syncInstanceFromInput(binding, el);
    if (syncStatus === "synced" || syncStatus === "cleared") {
      pushInputValue(binding, el);
    }
  }

  function parseMessageConfig(config) {
    if (!config) {
      return {};
    }
    if (typeof config === "string") {
      try {
        return JSON.parse(config);
      } catch (error) {
        return {};
      }
    }
    return config;
  }

  function getScriptConfig(el) {
    if (!el || !el.id) {
      return {};
    }
    var node = document.querySelector(
      'script[data-for="' + escapeSelectorValue(el.id) + '"]'
    );
    if (!node) {
      return {};
    }
    try {
      return JSON.parse(node.textContent || node.innerHTML || "{}");
    } catch (error) {
      return {};
    }
  }

  function getConfigStore(binding) {
    if (!binding.__ygConfigStore) {
      binding.__ygConfigStore = Object.create(null);
    }
    return binding.__ygConfigStore;
  }

  function mergeConfig(currentConfig, patchConfig) {
    var current = currentConfig || {};
    var patch = patchConfig || {};
    var merged = {};
    var key;

    for (key in current) {
      if (Object.prototype.hasOwnProperty.call(current, key) && key !== "options") {
        merged[key] = current[key];
      }
    }
    for (key in patch) {
      if (Object.prototype.hasOwnProperty.call(patch, key) && key !== "options") {
        merged[key] = patch[key];
      }
    }

    merged.options = {};
    if (current.options && typeof current.options === "object") {
      for (key in current.options) {
        if (Object.prototype.hasOwnProperty.call(current.options, key)) {
          merged.options[key] = current.options[key];
        }
      }
    }
    if (patch.options && typeof patch.options === "object") {
      for (key in patch.options) {
        if (Object.prototype.hasOwnProperty.call(patch.options, key)) {
          merged.options[key] = patch.options[key];
        }
      }
    }

    return merged;
  }

  function getConfig(binding, el) {
    var store = getConfigStore(binding);
    if (!store[el.id]) {
      store[el.id] = mergeConfig({}, getScriptConfig(el));
    }
    return store[el.id];
  }

  function updateStoredConfig(binding, el, patchConfig) {
    var store = getConfigStore(binding);
    store[el.id] = mergeConfig(getConfig(binding, el), patchConfig);
    return store[el.id];
  }

  function getConfigTimezone(config) {
    if (!config) {
      return null;
    }
    if (typeof config.tz === "string" && config.tz) {
      return config.tz;
    }
    if (config.options && typeof config.options.tz === "string" && config.options.tz) {
      return config.options.tz;
    }
    return null;
  }

  function normalizeFixedOffsetTimezone(tz) {
    return String(tz || "").trim().replace(/^YG/, "");
  }

  function isFixedOffsetTimezone(tz) {
    return /^UTC[+-]\d{2}:\d{2}$/.test(normalizeFixedOffsetTimezone(tz));
  }

  function parseFixedOffsetMinutes(tz) {
    var match = /^UTC([+-])(\d{2}):(\d{2})$/.exec(normalizeFixedOffsetTimezone(tz));
    if (!match) {
      return 0;
    }
    var sign = match[1] === "-" ? -1 : 1;
    return sign * (parseInt(match[2], 10) * 60 + parseInt(match[3], 10));
  }

  function parseDateValue(value) {
    if (value == null || value === "") {
      return null;
    }
    if (value instanceof Date) {
      return isNaN(value.getTime())
        ? null
        : { date: new Date(value.getTime()), dateOnly: false };
    }
    if (typeof value === "number") {
      var numericDate = new Date(value);
      return isNaN(numericDate.getTime())
        ? null
        : { date: numericDate, dateOnly: false };
    }
    if (typeof value === "string") {
      var trimmed = value.trim();
      if (!trimmed) {
        return null;
      }

      var dateOnlyMatch = /^(\d{4})-(\d{2})-(\d{2})$/.exec(trimmed);
      if (dateOnlyMatch) {
        return {
          date: new Date(
            parseInt(dateOnlyMatch[1], 10),
            parseInt(dateOnlyMatch[2], 10) - 1,
            parseInt(dateOnlyMatch[3], 10),
            0,
            0,
            0,
            0
          ),
          dateOnly: true
        };
      }

      var dateTimeMatch = /^(\d{4})-(\d{2})-(\d{2})[ T](\d{2}):(\d{2})(?::(\d{2}))?$/.exec(trimmed);
      if (dateTimeMatch) {
        return {
          date: new Date(Date.UTC(
            parseInt(dateTimeMatch[1], 10),
            parseInt(dateTimeMatch[2], 10) - 1,
            parseInt(dateTimeMatch[3], 10),
            parseInt(dateTimeMatch[4], 10),
            parseInt(dateTimeMatch[5], 10),
            parseInt(dateTimeMatch[6] || "0", 10),
            0
          )),
          dateOnly: false
        };
      }

      if (/^-?\d+(?:\.\d+)?$/.test(trimmed)) {
        var parsedNumber = new Date(Number(trimmed));
        return isNaN(parsedNumber.getTime())
          ? null
          : { date: parsedNumber, dateOnly: false };
      }

      var parsedDate = new Date(trimmed);
      return isNaN(parsedDate.getTime())
        ? null
        : { date: parsedDate, dateOnly: false };
    }
    return null;
  }

  function getTimezoneFormatter(timeZone) {
    var key = String(timeZone || "");
    if (!TIMEZONE_FORMATTER_CACHE[key]) {
      TIMEZONE_FORMATTER_CACHE[key] = new Intl.DateTimeFormat("en-CA", {
        hour12: false,
        timeZone: timeZone,
        year: "numeric",
        month: "2-digit",
        day: "2-digit",
        hour: "2-digit",
        minute: "2-digit",
        second: "2-digit"
      });
    }
    return TIMEZONE_FORMATTER_CACHE[key];
  }

  function getZonedDateParts(actualDate, timeZone) {
    try {
      var formatter = getTimezoneFormatter(timeZone);
      var rawParts = formatter.formatToParts(actualDate);
      var parts = {};

      rawParts.forEach(function(part) {
        if (
          part.type === "year" ||
          part.type === "month" ||
          part.type === "day" ||
          part.type === "hour" ||
          part.type === "minute" ||
          part.type === "second"
        ) {
          parts[part.type] = parseInt(part.value, 10);
        }
      });

      if (
        parts.year == null ||
        parts.month == null ||
        parts.day == null ||
        parts.hour == null ||
        parts.minute == null ||
        parts.second == null
      ) {
        return null;
      }

      if (parts.hour === 24) {
        parts.hour = 0;
      }

      return parts;
    } catch (error) {
      return null;
    }
  }

  function toDisplayDate(value, tz) {
    var parsedValue = parseDateValue(value);
    if (!parsedValue) {
      return null;
    }

    if (parsedValue.dateOnly) {
      return new Date(
        parsedValue.date.getFullYear(),
        parsedValue.date.getMonth(),
        parsedValue.date.getDate(),
        0,
        0,
        0,
        0
      );
    }

    if (!tz) {
      return parsedValue.date;
    }

    if (isFixedOffsetTimezone(tz)) {
      var shifted = new Date(
        parsedValue.date.getTime() + parseFixedOffsetMinutes(tz) * 60000
      );
      return new Date(
        shifted.getUTCFullYear(),
        shifted.getUTCMonth(),
        shifted.getUTCDate(),
        shifted.getUTCHours(),
        shifted.getUTCMinutes(),
        shifted.getUTCSeconds(),
        shifted.getUTCMilliseconds()
      );
    }

    var parts = getZonedDateParts(parsedValue.date, tz);
    if (!parts) {
      return parsedValue.date;
    }

    return new Date(
      parts.year,
      parts.month - 1,
      parts.day,
      parts.hour,
      parts.minute,
      parts.second,
      parsedValue.date.getUTCMilliseconds()
    );
  }

  function convertValuesForDisplay(values, tz) {
    if (!Array.isArray(values)) {
      return [];
    }

    return values.map(function(value) {
      return toDisplayDate(value, tz);
    }).filter(function(value) {
      return value instanceof Date && !isNaN(value.getTime());
    });
  }

  function padNumber(value, width) {
    var text = String(Math.abs(value));
    while (text.length < width) {
      text = "0" + text;
    }
    return text;
  }

  function formatDateForServer(date, includeTime) {
    var text = [
      padNumber(date.getFullYear(), 4),
      padNumber(date.getMonth() + 1, 2),
      padNumber(date.getDate(), 2)
    ].join("-");

    if (!includeTime) {
      return text;
    }

    return text + " " + [
      padNumber(date.getHours(), 2),
      padNumber(date.getMinutes(), 2),
      padNumber(date.getSeconds(), 2)
    ].join(":");
  }

  function getSelectedDates(instance) {
    if (!instance || !Array.isArray(instance.selectedDates)) {
      return [];
    }
    return instance.selectedDates.filter(function(date) {
      return date instanceof Date && !isNaN(date.getTime());
    });
  }

  function applyDatesToInstance(instance, dates) {
    if (!instance || typeof instance.selectDate !== "function") {
      return;
    }

    if (!dates.length) {
      if (typeof instance.clear === "function") {
        instance.clear({ silent: true });
      }
      return;
    }

    if (typeof instance.clear === "function") {
      instance.clear({ silent: true });
    }

    instance.selectDate(
      instance.opts && (instance.opts.range || instance.opts.multipleDates)
        ? cloneDates(dates)
        : new Date(dates[0].getTime()),
      { silent: true, updateTime: true }
    );
    syncInstanceView(instance, dates);
  }

  function setViewDate(instance, value, tz) {
    var displayDate = toDisplayDate(value, tz);
    if (!(displayDate instanceof Date) || isNaN(displayDate.getTime())) {
      return;
    }

    if (typeof instance.setViewDate === "function") {
      instance.setViewDate(new Date(displayDate.getTime()));
    } else {
      instance.viewDate = new Date(displayDate.getTime());
      instance.date = new Date(displayDate.getTime());
    }

    if (instance.nav && typeof instance.nav.render === "function") {
      instance.nav.render();
    }
    if (Array.isArray(instance.views)) {
      instance.views.forEach(function(view) {
        if (view && typeof view.render === "function") {
          view.render();
        }
      });
    }
  }

  function prepareOptions(options, tz) {
    if (!options || typeof options !== "object") {
      return {};
    }

    var prepared = {};
    Object.keys(options).forEach(function(key) {
      prepared[key] = options[key];
    });

    ["minDate", "maxDate", "startDate"].forEach(function(key) {
      if (Object.prototype.hasOwnProperty.call(prepared, key)) {
        prepared[key] = toDisplayDate(prepared[key], tz);
      }
    });

    return prepared;
  }

  function updateLabel(el, label) {
    var labelNode = document.getElementById(el.id + "-label");
    if (!labelNode) {
      return;
    }

    if (Array.isArray(label) && label.length === 0) {
      labelNode.classList.add("shiny-label-null");
      labelNode.innerHTML = "";
      return;
    }

    labelNode.classList.remove("shiny-label-null");
    labelNode.innerHTML = label == null ? "" : label;
  }

  function triggerChange(el) {
    if (window.jQuery) {
      window.jQuery(el).trigger("change");
      return;
    }
    el.dispatchEvent(new Event("change", { bubbles: true }));
  }

  function applyValueConfig(binding, el, valueConfig) {
    if (!binding || !binding.store) {
      return;
    }

    var patch = { value: valueConfig };
    if (valueConfig && Object.prototype.hasOwnProperty.call(valueConfig, "tz")) {
      patch.tz = valueConfig.tz;
    }

    var config = updateStoredConfig(binding, el, patch);
    var instance = binding.store[el.id];
    if (!instance) {
      return;
    }

    var tz = getConfigTimezone(config);
    var values = valueConfig && Array.isArray(valueConfig.value)
      ? valueConfig.value
      : [];
    applyDatesToInstance(instance, convertValuesForDisplay(values, tz));
  }

  function repairExistingInput(binding, el) {
    if (!binding || !binding.store || !binding.store[el.id]) {
      return;
    }

    var config = getConfig(binding, el);
    var tz = getConfigTimezone(config);
    var instance = binding.store[el.id];
    var preparedOptions = prepareOptions(config.options, tz);

    if (Object.keys(preparedOptions).length && typeof instance.update === "function") {
      instance.update(preparedOptions);
    }

    if (Array.isArray(config.value) && config.value.length) {
      applyDatesToInstance(instance, convertValuesForDisplay(config.value, tz));
    } else if (config.startView) {
      setViewDate(instance, config.startView, tz);
    } else if (config.options && config.options.startDate) {
      setViewDate(instance, config.options.startDate, tz);
    }
  }

  function installBindingPatch() {
    var binding = getBinding();
    if (!binding) {
      return false;
    }
    if (binding.__ygBindingPatchInstalled) {
      return true;
    }

    binding.__ygOriginalMethods = {
      initialize: binding.initialize,
      getType: binding.getType,
      getValue: binding.getValue,
      setValue: binding.setValue,
      receiveMessage: binding.receiveMessage
    };

    binding.initialize = function(el) {
      binding.__ygOriginalMethods.initialize.call(this, el);
      updateStoredConfig(binding, el, getScriptConfig(el));
      repairExistingInput(binding, el);
    };

    binding.getType = function(el) {
      var config = getConfig(binding, el);
      return config.options && config.options.timepicker
        ? "yg.air.datetime"
        : "yg.air.date";
    };

    binding.getValue = function(el) {
      var config = getConfig(binding, el);
      var instance = binding.store && binding.store[el.id];
      var selectedDates = getSelectedDates(instance);

      if (!selectedDates.length) {
        return null;
      }

      var includeTime = Boolean(config.options && config.options.timepicker);
      return {
        date: selectedDates.map(function(date) {
          return formatDateForServer(date, includeTime);
        }),
        tz: getConfigTimezone(config)
      };
    };

    binding.setValue = function(el, valueConfig) {
      applyValueConfig(binding, el, valueConfig);
    };

    binding.receiveMessage = function(el, message) {
      var instance = binding.store && binding.store[el.id];
      var configPatch = parseMessageConfig(message.config);
      var storePatch = configPatch;

      if (
        configPatch.value &&
        Object.prototype.hasOwnProperty.call(configPatch.value, "tz")
      ) {
        storePatch = mergeConfig(configPatch, { tz: configPatch.value.tz });
      }

      var config = updateStoredConfig(binding, el, storePatch);
      var tz = getConfigTimezone(config);

      if (instance && message.clear && typeof instance.clear === "function") {
        instance.clear({ silent: true });
      }
      if (instance && message.show && typeof instance.show === "function") {
        instance.show();
      }
      if (instance && message.hide && typeof instance.hide === "function") {
        instance.hide();
      }
      if (Object.prototype.hasOwnProperty.call(message, "label")) {
        updateLabel(el, message.label);
      }
      if (Object.prototype.hasOwnProperty.call(message, "placeholder")) {
        el.placeholder = message.placeholder || "";
      }

      if (instance && configPatch.options && typeof instance.update === "function") {
        instance.update(prepareOptions(configPatch.options, tz));
      }

      if (configPatch.value) {
        applyValueConfig(binding, el, configPatch.value);
      } else if (instance && configPatch.options && configPatch.options.startView) {
        setViewDate(instance, configPatch.options.startView, tz);
      }

      triggerChange(el);
    };

    binding.__ygBindingPatchInstalled = true;

    Array.prototype.forEach.call(
      document.querySelectorAll(".sw-air-picker"),
      function(el) {
        updateStoredConfig(binding, el, getScriptConfig(el));
        repairExistingInput(binding, el);
      }
    );

    return true;
  }

  function ensureBindingPatch() {
    if (installBindingPatch()) {
      return;
    }
    window.setTimeout(ensureBindingPatch, 50);
  }

  if (!window[PATCH_FLAG]) {
    document.addEventListener("change", handleManualSyncEvent, true);
    document.addEventListener("focusout", handleManualSyncEvent, true);
    ensureBindingPatch();
    window[PATCH_FLAG] = true;
  }
})();
