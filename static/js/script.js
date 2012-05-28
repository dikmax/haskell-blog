$(function () {
	$('.vault-posts-list tbody tr').click(function () {
		document.location = '/vault/edit/' + $(this).attr('data-rowid');
	});
});