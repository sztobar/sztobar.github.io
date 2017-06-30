(function() {
  var app = Elm.Board.fullscreen()

  app.ports.shuffleBoard.subscribe(function() {
    var x_len = 4
    var y_len = 6
    var pts = make_pts(x_len, y_len)
    var shuffled_pts = shuffle(pts.slice())
    app.ports.newBoard.send(shuffled_pts)
  })

  function make_pts(x_len, y_len) {
    var pts = []

    for (var x = 0; x < x_len; x++) {
      for (var y = 0; y < y_len; y++) {
        pts.push([y, x])
      }
    }
    return pts
  }

  function shuffle(arr) {
    var index = -1
    var len = arr.length
    var lastIndex = len - 1

    while (++index < len) {
      var rand = random(index, lastIndex)

      var tmp = arr[rand]
      arr[rand] = arr[index]
      arr[index] = tmp
    }

    return arr
  }

  function random(from, to)  {
    return from + Math.floor(Math.random() * (to - from + 1))
  }

}())
