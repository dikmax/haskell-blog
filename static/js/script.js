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
});