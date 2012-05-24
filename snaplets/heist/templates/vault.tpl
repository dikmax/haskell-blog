<!DOCTYPE html>
<html lang="en">
<apply template="head" />
<body>
	<apply template="topnav" />
	<div class="container">
	    <div class="well">
	    	Welcome to vault!
	    	<form method="post" action="/vault">
				<input type="hidden" name="action" value="logout" />	    	 
		    	<button class="btn btn-inverse" type="submit">
				  Logout
				</button>
	    	</form>
	    </div>
		<apply template="footer" />
	</div>
	<apply template="foot" />
</body>
</html>