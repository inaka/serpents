function gameId() {
  var parts = window.location.href.split("/");
  return parts[parts.length - 1];
};

window.players = [];
window.playerColors = {};
window.rounds_count = 0;

window.entityMap = {
  "&": "&amp;",
  "<": "&lt;",
  ">": "&gt;",
  '"': '&quot;',
  "'": '&#39;',
  "/": '&#x2F;'
};

function escapeHtml(string) {
  return String(string).replace(/[&<>"'\/]/g, function (s) {
    return window.entityMap[s];
  });
}
function nameToColor(name) {
  if(!window.playerColors[name]) {
    window.playerColors[name] = '#' + CryptoJS.MD5(name).toString().substring(0, 6);
  }
  return  window.playerColors[name];
}
function setupPlayers(players) {
  $('.player-list ul').empty();

  players.forEach(function(player) {
    addPlayer(player);
  })
}
function addPlayer(player) {
  var playerName = escapeHtml(player.name);
  $('.player-list ul').append(
    '<li><div class="player-color" style="background-color: ' + nameToColor(playerName) + '"></div>' + playerName + '</li>'
  );
}
function setupBoard(game) {
  var rows = game.rows,
      cols = game.cols,
      i, j;
  $('table.board').empty();
  for(i = 1; i <= rows; i++) {
    var htmlRow = '<tr>';

    for(j = 1; j <= cols; j++) {
      htmlRow += '<td id="cell_' + i + '_' + j + '"></td>'
    }
    htmlRow += '</tr>';
    $('table.board').append(htmlRow);
  }
  if(game.rounds) {
    $('#total_rounds').text(game.rounds);
    $('.rounds').show();
  }
}
function redrawPlayer(coords, color) {
  coords.forEach(function(coord) {
    paintCell(coord[0], coord[1], color)
  })
}
function fadeoutPlayer(coords) {
  coords.forEach(function(coord) {
    findCell(coord[0], coord[1]).animate({backgroundColor: '#eee'}, 250);
  })
}

function redrawStatus(game) {
  var countdown = document.getElementById('countdown'),
      status  = document.getElementById('status');
  countdown.innerHTML = (game.countdown * game.ticktime / 1000).toFixed(2);
  status.innerHTML = game.state;
}
function redrawBoard(game) {
  var alivePlayers;
  window.players.forEach(function(player) {
    redrawPlayer(player.body, null);
  });

  alivePlayers = game.serpents.filter(function(player) {
    return player.status === 'alive';
  });
  alivePlayers.forEach(function(player) {
    redrawPlayer(player.body, nameToColor(player.name));
  });
  window.players = game.serpents;

  game.cells.forEach(function(cell) {
    switch(cell.content) {
      case 'fruit':
        paintCell(cell.row, cell.col, '#f00');
        break;
      case 'wall':
        paintCell(cell.row, cell.col, '#000');
        break;
    }
  });
}

function redrawGame(game) {
  redrawStatus(game);
  redrawBoard(game);
}
function findCell(row, col) {
  return $('#cell_' + row + '_' + col);
}
function paintCell(row, col, color) {
  var cell = document.getElementById(
    'cell_' + row + '_' + col
  );
  if(cell) {
    cell.style['background-color'] = color;
  }
}
$(document).ready(function() {
  var id = gameId(),
      evtSource,
      processBoard,
      processBoardWithRound;

  $("#game_id").text(id);
  $("#start_game").click(function() {
    var data = {state: 'started'};
    $.ajax('/api/games/' + id,
      {
        data : JSON.stringify(data),
        contentType : 'application/json',
        type: 'PUT',
        success: function(game) {
          redrawGame(game);
        },
        error: function(response) {
          alert(
            'An error was found while creating the game: ' +
            response.responseJSON.error
          );
        }
      }
    );
  });
  evtSource = new EventSource('/api/games/' + id + '/news');
  processBoard = function(e) {
    var game = JSON.parse(e.data);
    redrawGame(game);
  };
  processBoardWithRound = function(e) {
    var game = JSON.parse(e.data);
    window.rounds_count++;
    $('#rounds_count').text(window.rounds_count);
    redrawGame(game);
  };

  evtSource.addEventListener("game_updated", processBoardWithRound, false);
  evtSource.addEventListener("game_countdown", processBoard, false);
  evtSource.addEventListener("game_started", processBoard, false);
  evtSource.addEventListener("game_status", processBoard, false);
  evtSource.addEventListener("game_finished", processBoardWithRound, false);
  evtSource.addEventListener("serpent_added", function(e) {
    var player = JSON.parse(e.data);
    addPlayer(player);
    redrawPlayer(player.body, nameToColor(player.name))
  }, false);
  evtSource.addEventListener("collision_detected", function(e) {
    var player = JSON.parse(e.data);
    fadeoutPlayer(player.body);
  }, false);
  $.getJSON('/api/games/' + id, function(game) {
    setupPlayers(game.serpents);
    setupBoard(game);
    redrawGame(game);
  });
});

