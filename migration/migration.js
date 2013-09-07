var mysql = require('mysql'),
    fs = require('fs'),
    moment = require('moment');

var folder = '../posts/';
var connection = mysql.createConnection({
  host: 'localhost',
  user: 'root',
  password: '',
  database: 'haskellblog'
});

connection.connect(function(err) {
  if (err) {
    console.log(err);
    return;
  }

  connection.query('SELECT * FROM posts', function(err, rows) {
    if (err) {
      console.log(err);
      return;
    }
    rows.forEach(function (row) {
      var date = moment(row.date);
      var content = '---\n' +
          'title: ' + row.title + '\n' +
          'date: ' + date.format() + '\n' +
          (row.published ? '' : 'published: false\n') +
          (row.special ? 'special: true\n' : '') +
          (row.tags ? 'tags: ' + row.tags + '\n' : '') +
          '---\n\n' +
          row.text;

      var path = folder + date.format('YYYY-MM-DD') + '-' + row.url + '.md';
      if (row.url == 'shoutbox' || row.url == 'about' || row.url == 'latest') {
        path = '../' + row.url + '.md';
      }
      fs.writeFileSync(path, content);
    });
    connection.end();
  });
});