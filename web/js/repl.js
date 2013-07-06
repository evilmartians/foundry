$(function() {
  var editor = CodeMirror($('#repl-editor')[0], {
    value: "def foo() : Integer\n  2 + 2\nend",

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
      }
    }
  });

  var pos = CodeMirror.Pos;

  $("#repl-run").click(function() {
    editor.addLineWidget(1, $('<span style="">foo</span>')[0])
  });
});
