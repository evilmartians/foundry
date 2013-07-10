/*
 * === HIGHLIGHT ===
 */

function Highlight(manager, order, events) {
  this.manager    = manager;
  this.editor     = manager.editor;
  this.order      = order;
  this.events     = events;
  this.lineWidget = null;

  this.cmMark     = null;
  this.cmWidget   = null;
}

Highlight.prototype = {
  draw: function(cmRange, isWatching) {
    if(this.cmMark) {
      this.cmMark.clear();
    }

    this.cmMark = this.editor.markText(
      cmRange.from, cmRange.to,
      { className: 'diag diag-' + this.order,
        inclusiveLeft:  isWatching,
        inclusiveRight: isWatching }
    );

    this.isWatching      = isWatching;
    this.lastFoundOnLine = cmRange.from.line;

    var $this = this;

    if(this.isWatching) {
      CodeMirror.on(this.cmMark, 'beforeCursorEnter', function() {
        setTimeout(function() {
          $this.redraw(false);
          $this.manager.restoreWatchingExcept($this);

          if($this.events.onEnter) {
            $this.events.onEnter();
          }
        }, 0);
      });
    }

    CodeMirror.on(this.cmMark, 'hide', function() {
      $this.manager.updateWidgets();
    });

    CodeMirror.on(this.cmMark, 'unhide', function() {
      $this.manager.updateWidgets();
    });
  },

  redraw: function(isWatching) {
    if(this.isWatching != isWatching) {
      this.draw(this.cmMark.find(), isWatching);
    }
  },

  line: function() {
    if(cmRange = this.cmMark.find()) {
      return cmRange.from.line;
    } else {
      return undefined;
    }
  },

  lineChanged: function() {
    return this.line() != this.lastFoundOnLine;
  },

  drawWidgets: function() {
    var line = this.line();

    if(line !== undefined) {
      var marker = $('<div class="diag-marker diag-marker-' + this.order + '"></div>');
      this.editor.setGutterMarker(line, 'diag-gutter', marker[0]);
    }

    this.drawLineWidget();

    this.lastFoundOnLine = line;
  },

  drawLineWidget: function() {
    if(this.cmWidget) {
      this.cmWidget.clear();
    }

    var line = this.line();

    if(line !== undefined && this.lineWidget) {
      var widget = this.lineWidget();
      this.cmWidget = this.editor.addLineWidget(line, widget);
    }
  },

  clear: function() {
    this.cmMark.clear();

    if(this.cmWidget) {
      this.cmWidget.clear();
    }
  },

  /* public */
  focus: function() {
    this.editor.setCursor(this.cmMark.find().from);
    this.editor.focus();
  },

  setLineWidget: function(lineWidget) {
    this.lineWidget = lineWidget;
    this.drawLineWidget();
  }
}

/*
 * === HIGHLIGHT MANAGER ===
 */

function HighlightManager(editor) {
  this.editor     = editor;
  this.highlights = [];

  var $this = this;
  this.editor.on("change", function() {
    $this.updateWidgets();
  })
};

HighlightManager.prototype = {
  /* private */
  restoreWatchingExcept: function(currentHighlight) {
    for(var i = 0; i < this.highlights.length; i++) {
      var highlight = this.highlights[i];

      if(highlight == currentHighlight) {
        continue;
      }

      highlight.redraw(true);
    }
  },

  updateWidgets: function(needsUpdate) {
    for(var i = 0; i < this.highlights.length; i++) {
      needsUpdate = needsUpdate || this.highlights[i].lineChanged();
    }

    if(needsUpdate) {
      this.editor.clearGutter('diag-gutter');
      for(var i = 0; i < this.highlights.length; i++) {
        this.highlights[i].drawWidgets();
      }
    }
  },

  /* public */

  /* addMark({from, to}, order, {onEnter}) → Highlight */
  addHighlight: function(range, order, events) {
    var highlight = new Highlight(this, order, events);

    var cmRange = {
      from: this.editor.posFromIndex(range.from),
      to:   this.editor.posFromIndex(range.to)
    }

    highlight.draw(cmRange, true);
    highlight.drawWidgets();

    this.highlights.push(highlight);

    return highlight;
  },

  removeHighlight: function(highlight) {
    for(var i = 0; i < this.highlights.length; i++) {
      if(this.highlights[i] == highlight) {
        this.highlights.splice(i, 1);
        break;
      }
    }

    highlight.clear();
    this.updateWidgets(true);
  },

  clear: function() {
    for(var i = 0; i < this.highlights.length; i++) {
      this.highlights[i].clear();
    }

    this.highlights = [];
    this.updateWidgets();
  }
}

/*
 * === DIAGNOSTIC ===
 */

function Diagnostic(manager, message, locations) {
  this.manager    = manager;
  this.highlights = manager.highlights;
  this.message    = message;
  this.locations  = locations;

  this.expanded   = false;

  this.mainHighlight       = null;
  this.transientHighlights = [];
}

Diagnostic.prototype = {
  draw: function() {
    var $this = this;

    this.mainHighlight =
      this.highlights.addHighlight(this.locations[0], 0, {
        onEnter: function() {
          $this.manager.collapseAll();
          $this.expand();
        }
      });
  },

  expand: function() {
    if(this.expanded) {
      return;
    }

    this.expanded = true;

    var $this = this;

    this.mainHighlight.setLineWidget(function() {
      var widget = $('<div class="diag-message"></div>');
      widget.text($this.message);
      return widget[0];
    })

    for(var i = 1; i < this.locations.length; i++) {
      var location  = this.locations[i];
      var highlight = this.highlights.addHighlight(location, i, {});
      this.transientHighlights.push(highlight);
    }
  },

  collapse: function() {
    if(!this.expanded) {
      return;
    }

    this.expanded = false;

    this.mainHighlight.setLineWidget(null);

    for(var i = 0; i < this.transientHighlights.length; i++) {
      var highlight = this.transientHighlights[i];
      this.highlights.removeHighlight(highlight);
    }

    this.transientHighlights = [];
  },

  focus: function() {
    this.mainHighlight.focus();
    this.expand(); /* if the cursor was already in the marked
                      area, onEnter won't happen */
  },

  clear: function() {
    this.collapse();
    this.highlights.removeHighlight(this.mainHighlight);
  }
}

/*
 * === DIAGNOSTIC MANAGER ===
 */

function DiagnosticManager(highlights) {
  this.highlights  = highlights;
  this.diagnostics = [];
}

DiagnosticManager.prototype = {
  addDiagnostic: function(message, locations) {
    var diagnostic = new Diagnostic(this, message, locations);
    this.diagnostics.push(diagnostic);

    diagnostic.draw();

    return diagnostic;
  },

  collapseAll: function() {
    for(var i = 0; i < this.diagnostics.length; i++) {
      this.diagnostics[i].collapse();
    }
  },

  clear: function() {
    for(var i = 0; i < this.diagnostics.length; i++) {
      this.diagnostics[i].clear();
    }

    this.diagnostics = [];
  }
}

$(function() {
  var editor = CodeMirror($('#repl-editor')[0], {
    value: (window.localStorage["code"] ||
      "class A;\n  def @foo : Symbol;\n\n  def @foo : Integer;\nend"),

    mode:               'foundry',
    theme:              'monokai',
    autofocus:          true,
    lineNumbers:        true,
    matchBrackets:      true,
    autoCloseBrackets:  true,
    styleActiveLine:    true,
    gutters:            ['diag-gutter'],

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

  var highlights  = new HighlightManager(editor);
  var diagnostics = new DiagnosticManager(highlights);

  window.e = editor;
  window.d = diagnostics;
  window.h = highlights;

  $("#repl-output").hide();
  $("#repl-output .close").click(function() {
    $("#repl-output").hide();
  });

  function analyze(gotoError) {
    /* Reset */
    diagnostics.clear();

    /* Analyze */
    var code = editor.getValue(), result;

    try {
      var result = foundryProcess(code, false);
    } catch(e) {
      return;
    }

    /* Display */
    if(result.type == "diagnostics") {
      result.value.forEach(function(desc, index) {
        var diagnostic = diagnostics.addDiagnostic(desc.message, desc.locations);

        if(gotoError && index == 0){
          diagnostic.focus();
        } else {
          var cursor  = editor.indexFromPos(editor.getCursor());
          var mainLoc = desc.locations[0];

          if(cursor >= mainLoc.from && cursor <= mainLoc.to) {
            diagnostic.expand();
          }
        }
      });
    }
  }

  var analyzeTimer = null;

  editor.on('change', function() {
    if(analyzeTimer) {
      clearTimeout(analyzeTimer);
    }

    analyzeTimer = setTimeout(function() {
      analyze();
      analyzeTimer = null;
    }, 300);
  })

  function compile() {
    /* Reset */
    diagnostics.clear();
    $("#repl-output").hide().removeClass("alert-success alert-error");

    /* Compile */
    var code = editor.getValue(), result;

    window.localStorage["code"] = code;

    try {
      result = foundryProcess(code, true);
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
      result.value.forEach(function(desc, index) {
        var diagnostic = diagnostics.addDiagnostic(desc.message, desc.locations);

        if(index == 0) {
          diagnostic.focus();
        }
      });
    } else if(result.type == "error") {
      $("#repl-output .output").text("Shit broke in a mundane way: " + result.value);
      $("#repl-output").addClass("alert-error").show();
    }
  };

  analyze(true);
  $("#repl-run").click(compile);
});
