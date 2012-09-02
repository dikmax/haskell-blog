<!DOCTYPE html>
<html lang="ru">
	<apply template="head" />
<body itemscope="itemscope" itemtype="http://schema.org/WebPage">
    <apply template="author" />
	<apply template="topnav" />
	<div class="container">
		<div class="component-panel latest-movies">
			<h1 itemprop="name">Последние просмотренные фильмы</h1>
			<latest />
		      <div id="disqus_thread"></div>
		      <script type="text/javascript">
		        var disqus_shortname = 'dikmax';

		        (function() {
		          var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
		          dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
		          (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
		        })();
		      </script>
		      <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
		      <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>
		</div>
		<apply template="footer" />				
	</div>	
    	
	<apply template="foot" />
</body>
</html>