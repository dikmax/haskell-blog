<!DOCTYPE html>
<html lang="en">
<apply template="head" />
<body>
	<apply template="vaulttopnav" />
	<div class="container">
	    <a class="btn" href="/vault/edit">Новая запись</a>
	    <a class="btn" href="/vault/files">Управление файлами</a>
	    <table class="table table-striped vault-posts-list">
	    	<colgroup>
	    		<col width="100" />
	    		<col width="30" />
	    		<col />
	    		<col width="80" />
	    	</colgroup>
	    	<thead>
	    		<tr>
	    			<th>Дата</th>
	    			<th>&nbsp;</th>
	    			<th>Заголовок</th>
	    			<th>&nbsp;</th>
	    		</tr>
	    	</thead>
	    	<tbody>
	    	    <posts />
	    	</tbody>
	    </table>
		<apply template="footer" />
	</div>
	<apply template="foot" />
</body>
</html>