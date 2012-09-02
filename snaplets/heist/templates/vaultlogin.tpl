<!DOCTYPE html>
<html lang="ru">
<apply template="head" />
<body>
	<apply template="topnav" />
	<div class="container">
		<div class="span4 offset4">
			<form class="well form-horizontal" method="post" action="">
				<legend>Welcome to Vault.</legend>
				<error></error>
				<input type="hidden" name="action" value="login" />
				<input type="text" name="login" style="width: 100%; position: relative; left: -4px;" placeholder="Email">
				<input type="password" name="password" style="width: 100%; position: relative; left: -4px;"  placeholder="Password">
				<button type="submit" class="btn" style="width: 100%;">Sign in</button>
			</form>
		</div>
		<div class="clearfix"></div>
		<apply template="footer" />
	</div>
	<apply template="foot" />
</body>
</html>