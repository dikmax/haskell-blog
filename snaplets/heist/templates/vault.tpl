<!DOCTYPE html>
<html lang="ru">
<apply template="vaulthead" />
<body>
	<apply template="vaulttopnav" />
	<div class="container">
        <div class="pull-right vault-statistics">
            <span class="label label-info vault-posts-count"></span><br />
            <span class="label label-success vault-published-count"></span><br />
            <span class="label label-warning vault-filtered-count"></span>
        </div>
        <a class="btn" href="/vault/edit">Новая запись</a>
        <a class="btn" href="/vault/files">Управление файлами</a>
            <input type="text" class="input-medium search-query vault-posts-list-filter" placeholder="Фильтр" />

        <table class="table table-striped table-hover vault-posts-list">
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
	    	</tbody>
	    </table>
		<apply template="footer" />
	</div>
	<apply template="vaultfoot" />
</body>
</html>