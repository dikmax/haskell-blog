<!DOCTYPE html>
<html lang="en">
<apply template="head" />
<body>
	<apply template="vaulttopnav" />
	<div class="container">
	    <a class="btn" href="/vault/edit">Новая запись</a>
	    <table class="table table-striped">
	    	<colgroup>
	    		<col width="100" />
	    		<col width="30" />
	    		<col />
	    	</colgroup>
	    	<thead>
	    		<tr>
	    			<th>Дата</th>
	    			<th>&nbsp;</th>
	    			<th>Заголовок</th>
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