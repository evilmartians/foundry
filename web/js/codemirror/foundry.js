CodeMirror.defineMode('foundry', function(config) {
  function lookupObj(words) {
    var o = {};
    for (var i = 0, e = words.length; i < e; ++i) o[words[i]] = true;
    return o;
  }
  var keywords = lookupObj([
    "true", "false", "nil", "self", "and", "or", "not", "let", "mut", "while",
    "do", "if", "elsif", "then", "else", "match", "end", "as", "meta", "type",
    "public", "dynamic", "package", "class", "mixin", "iface", "def", "return"
  ]);
  var indentWords = lookupObj([
    "then", "do", "class", "package", "mixin", "iface"
  ]);
  var dedentWords = lookupObj([
    "end", "}"
  ]);

  return {
    startState: function() {
      return {
        indentation: 0,
        inDef:       false
      };
    },

    token: function(stream, state) {
      if(stream.sol()) {
        state.indentation = stream.indentation();
      }

      if(stream.match(/[ \t]+/, true)) {
        return null; // whitespace
      }

      var wasInDef = state.inDef;
      state.inDef = false;

      if(stream.match(/([+*\/%&|<>~-]|<<|>>>?|and|or)=?|==|<=>|[+~-]@/, true)) {
        return "operator";
      } else if(stream.match(/[{}\[\]()]/, true)) {
        if(stream.current() == "{") {
          state.indentation += config.indentUnit;
        }

        return "bracket";
      } else if(stream.match(/\d+/, true) && !stream.match(/[a-zA-Z_]/)) {
        return "number";
      } else if(stream.match(/[A-Za-z_][A-Za-z_0-9]*:/, true)) {
        return "attribute";
      } else if(stream.match(/@[A-Za-z_][A-Za-z_0-9]*/, true)) {
        return "variable";
      } else if(stream.match(/(\\[A-Za-z_]|[A-Z])[A-Za-z_0-9]*/, true)) {
        return "variable-2";
      } else if(match = stream.match(/([a-z_][A-Za-z_0-9]*)/, true)) {
        if(keywords[match[0]] && !wasInDef) {
          if(indentWords[stream.current()]) {
            state.indentation += config.indentUnit;
          }

          if(match[0] == "def") {
            state.inDef = true;
          }

          return "keyword";
        } else {
          if(wasInDef) {
            state.indentation += config.indentUnit;
            return "variable";
          } else {
            return null;
          }
        }
      } else if(stream.match(/:(([+*\/%&|<>~-]|<<|>>>?|and|or)=?|==|<=>|[+~-]@)/, true) ||
                stream.match(/:[A-Za-z_][A-Za-z_0-9]*/)) {
        return "atom";
      } else if(stream.match(/[=.,:;]|->|=>/, true)) {
        return null; // punctuation
      } else if(stream.eat('#')){
        stream.skipToEnd();
        return "comment";
      } else {
        stream.eat(/./);
        return "error";
      }
    },

    indent: function(state, textAfter) {
      if(dedentWords[textAfter]) {
        return state.indentation - config.indentUnit;
      } else {
        return state.indentation;
      }
    },

    electricChars: "}d", /* end */
    lineComment:   "#"
  };
});
