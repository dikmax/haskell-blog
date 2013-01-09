<!DOCTYPE html>
<html lang="ru">
<apply template="vaulthead" />
<body>
	<apply template="topnav" />
	<!--<div class="container">
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
	</div>-->
    <div class="container">

        <form class="well form-signin" method="post" action="">
            <h2 class="form-signin-heading">Welcome to Vault.</h2>
            <error></error>
            <input type="hidden" name="action" value="login" />
            <input type="text" name="login" class="input-block-level" placeholder="Login">
            <input type="password" name="password" class="input-block-level" placeholder="Password">
            <button class="btn btn-large btn-primary" type="submit">Sign in</button>
        </form>

    </div>
	<apply template="vaultfoot" />
</body>
</html>