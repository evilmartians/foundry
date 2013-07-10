CodeMirror.defineMode('foundry', function(config) {
  function lookupObj(words) {
    var o = {};
    for (var i = 0, e = words.length; i < e; ++i) o[words[i]] = true;
    return o;
  }
  var keywords = lookupObj([
    "true", "false", "nil", "self", "and", "or", "not", "let", "mut", "while",
    "do", "if", "elsif", "then", "else", "match", "end", "as", "meta", "type",
    "public", "dynamic", "package", "class", "mixin", "iface", "def", "return",
    "invokeprimitive"
  ]);
  var indentWords = lookupObj([
    "then", "do", "class", "package", "mixin", "iface"
  ]);
  var dedentWords = lookupObj([
    "end", "}"
  ]);

  function indent(state) {
    state.indentation += config.indentUnit;
    state.indentedLine = true;
  }
  function dedent(state) {
    state.indentation -= config.indentUnit;
  }

  return {
    startState: function() {
      return {
        indentation:  0,
        indentedLine: false,
        inDef:        false
      };
    },

    token: function(stream, state) {
      if(stream.sol()) {
        state.indentation  = stream.indentation();
        state.indentedLine = false;
      }

      if(stream.match(/[ \t]+/, true)) {
        return null; // whitespace
      }

      var wasInDef = state.inDef;
      state.inDef = false;

      if(stream.match(/([+*\/%&|<>~-]|<<|>>>?)=?|==|<=>|[+~-]@/, true)) {
        if(wasInDef) {
          indent(state);
          return "variable";
        }

        return "operator";
      } else if(stream.match(/and=|or=/, true)) {
        return "operator";
      } else if(stream.match(/[{}\[\]()]/, true)) {
        if(stream.current() == "{") {
          indent();
        } else if(stream.current() == "}" && state.indentedLine) {
          dedent();
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
            indent(state);
          } else if(dedentWords[stream.current()] && state.indentedLine) {
            dedent(state);
          }

          if(match[0] == "def") {
            state.inDef = true;
          }

          return "keyword";
        } else {
          if(wasInDef) {
            indent(state);
            return "variable";
          } else {
            return null;
          }
        }
      } else if(stream.match(/:(([+*\/%&|<>~-]|<<|>>>?)=?|==|<=>|[+~-]@)/, true) ||
                stream.match(/:[A-Za-z_][A-Za-z_0-9]*/)) {
        return "atom";
      } else if(stream.eat('.')) {
        state.inDef = true;

        return null;
      } else if(stream.match(/[=,:;]|->|=>/, true)) {
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
