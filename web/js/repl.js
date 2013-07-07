$(function() {
  var editor = CodeMirror($('#repl-editor')[0], {
    value: //"def foo() : Integer\n  2 + 2\nend",
      "class A;\n  def @foo : Symbol;\n\n  def @foo : Integer;\nend",

    mode:               'foundry',
    theme:              'monokai',
    autofocus:          true,
    lineNumbers:        true,
    matchBrackets:      true,
    autoCloseBrackets:  true,
    styleActiveLine:    true,

    extraKeys: {
      "Tab": function(cm) {
        var spaces = Array(cm.getOption("indentUnit") + 1).join(" ");
        cm.replaceSelection(spaces, "end", "+input");
      },
      "Ctrl-/": function(cm) {
        var from = cm.getCursor("start"), to = cm.getCursor("end");
        cm.uncomment(from, to) || cm.lineComment(from, to, {indent: true});
      },
      "Ctrl-Enter": function(cm) {
        compile();
      }
    }
  });

  $("#repl-output").hide();
  $("#repl-output .close").click(function() {
    $("#repl-output").hide();
  });

  function mark(loc, index) {
    function linech_of_pos(pos) {
      return editor.posFromIndex(pos);
    }

    var range = editor.markText(
        linech_of_pos(loc.from), linech_of_pos(loc.to),
        { className: 'diag diag-' + index,
          inclusiveLeft:  true,
          inclusiveRight: true });
    return range;
  }

  function renderDiagnosticWidget(diagnostic) {
    var widget = $('<div class="diag-message"></div>');
    widget.text(diagnostic.message);
    return widget[0];
  }

  var currentDiagnostic = null;

  editor.on("change", function() {
    if(currentDiagnostic) {
      var range = currentDiagnostic.mainMark.find();
      if(range) {
        var line = range.to.line;
        if(editor.getLineNumber(currentDiagnostic.widget.line) != line) {
          currentDiagnostic.widget.clear();

          var widget = renderDiagnosticWidget(currentDiagnostic.source);
          currentDiagnostic.widget = editor.addLineWidget(line, widget);
        }
      } else {
        currentDiagnostic.widget.clear();
      }
    }
  });

  function compile() {
    /* Reset */
    $("#repl-output").hide().removeClass("alert-success alert-error");

    if(currentDiagnostic && currentDiagnostic.widget) {
      currentDiagnostic.widget.clear();
    }
    currentDiagnostic = null;

    editor.getAllMarks().forEach(function(mark) {
      mark.clear();
    });

    /* Compile */
    var code = editor.getValue(), result;
    try {
      result = foundryEval(code);
    } catch(e) {
      $("#repl-output .output").text("Shit broke really hard: " + e.toString());
      $("#repl-output").addClass("alert-error").show();
      return;
    }

    /* Display */
    if(result.type == "output") {
      $("#repl-output .output").text(result.value);
      $("#repl-output").addClass("alert-success").show();
    } else if(result.type == "diagnostics") {
      diagnostics = result.value;
      diagnostics.forEach(function(diagnostic, index) {
        if(diagnostic.locations.length > 0) {
          var range = mark(diagnostic.locations[0], 1);

          CodeMirror.on(range, "beforeCursorEnter", function() {
            if(currentDiagnostic) return;

            currentDiagnostic = {
              transientMarks: [],
              widget:         null,
              mainMark:       range,
              source:         diagnostic
            };

            for(var i = 1; i < diagnostic.locations.length; i++) {
              currentDiagnostic.transientMarks.
                push(mark(diagnostic.locations[i], i + 1));
            }

            var widget = renderDiagnosticWidget(diagnostic);
            var line = range.find().to.line;
            currentDiagnostic.widget = editor.addLineWidget(line, widget);
          });

          if(index == 0) {
            editor.setCursor(range.find().from);
            editor.focus();
          }
        }
      });
    } else if(result.type == "error") {
      $("#repl-output .output").text("Shit broke in a mundane way: " + result.value);
      $("#repl-output").addClass("alert-error").show();
    }
  };

  $("#repl-run").click(compile);
});
