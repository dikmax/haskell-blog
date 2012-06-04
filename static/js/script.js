$(function() {
	$('.vault-posts-list tbody tr').click(function() {
		document.location = '/vault/edit/' + $(this).attr('data-rowid');
	});

	$('.vault-posts-list span.action-delete').click(function() {
		var id = $(this).parents('tr').attr('data-rowid');
		if (confirm('Действительно удалить запись ' + id + '?')) {
			document.location = '/vault/delete/' + id;
		}
		return false;
	});

	$('.vault-posts-list span.action-view').click(function() {
		var url = $(this).parents('tr').attr('data-url');
		document.location = '/post/' + url;
		return false;
	});
	
	$('p.post-comments > a').bind("DOMCharacterDataModified", function (e) {
		var text = e.target.innerText;
		var match = text.match(/^(\d+) комментариев/);
		if (match) {
			var count = Number(match[1]);
			if (count % 100 != 1) {
				var count10 = count % 10;
				if (count10 == 1) {
					e.target.innerText = count + ' комментарий';
				} else if (count10 >= 2 && count10 <= 4) {
					e.target.innerText = count + ' комментария';
				}
			}
		}
	});
});