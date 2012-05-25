<!DOCTYPE html>
<html lang="en">
<apply template="head" />
<body>
	<apply template="vaulttopnav" />
	<div class="container">
		<a class="btn" href="/vault">Все записи</a>
		<form class="form-horizontal">
			<fieldset>
				<legend>Редактирование записи</legend>
				<div class="control-group">
					<label class="control-label" for="postTitle">Заголовок</label>
					<div class="controls">
						<input type="text" name="title" class="input-xxlarge" id="postTitle">
					</div>
				</div>

				<div class="control-group">
					<label class="control-label" for="postUrl">Url</label>
					<div class="controls">
						<input type="text" name="url" class="input-xxlarge" id="postUrl">
					</div>
				</div>

				<div class="control-group">
					<label class="control-label" for="postDate">Дата</label>
					<div class="controls">
						<input type="text" name="date" class="input-xxlarge" id="postDate">
					</div>
				</div>

				<div class="control-group">
					<label class="control-label" for="postPublished">Опубликовано</label>
					<div class="controls">
						<input type="checkbox" name="published" id="postPublished">
					</div>
				</div>

				<div class="control-group">
					<label class="control-label" for="postSpecial">Специальный</label>
					<div class="controls">
						<input type="checkbox" name="special" id="postSpecial">
					</div>
				</div>

				<div class="control-group">
					<label class="control-label" for="postText">Текст</label>
					<div class="controls">
						<textarea name="text" id="postText" class="input-xxlarge" rows="20"></textarea>
					</div>
				</div>
			</fieldset>
		</form>
		<apply template="footer" />
	</div>
	<apply template="foot" />
</body>
</html>