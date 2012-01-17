
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />

<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-1088763-8']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>

  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="https://r-forge.r-project.org/themes/rforge/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<p> The <strong>stable dclone release</strong> you can find <a href="http://cran.r-project.org/web/packages/dclone/index.html"><strong>at CRAN</strong></a>. </p>

<h2>Documentation</h2>
<p>
<ul>
<li>Short introduction: P&eacute;ter S&oacute;lymos, 2010. dclone: Data Cloning in R. 
<i>The R Journal</i>, 2(2):29-37, December 2010. URL <code>http://journal.R-project.org/</code> 
[<A href="http://journal.r-project.org/archive/2010-2/RJournal_2010-2_bib.html#RJournal:2010-2:Solymos">bib</A>] 
[<A href="http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf">pdf</A>] 
[<A href="http://dcr.r-forge.r-project.org/publications/RJournal_2010-2_Solymos.pdf">local copy</A>]
[<A href="http://dcr.r-forge.r-project.org/publications/RJournal_2010-2_Solymos.R">R code</A>]<i> (Official citation)</i>
</li><li>Reference manual: [<A href="http://cran.r-project.org/web/packages/dclone/dclone.pdf">pdf</A>] <i>(Detailed technical documentation of the package from CRAN)</i></li>
<li>Additional resources: <a href="http://dcr.r-forge.r-project.org/tutorials/">tutorials</a> (under development), <a href="http://dcr.r-forge.r-project.org/examples/">worked examples</a> (also available via the <strong>dcmle</strong> package).</li>
</ul>
</p>

<h2>Infrastructure for dclone</h2>

<p>Because <strong>dclone</strong> is an <strong>R</strong> package, installing R is absolutely necessary. 
The choice of the preferred <strong>BUGS</strong> program is up to the user.</p>

<ul>
<li><strong>R (>= 2.14.0):</strong> download from 
<a href="http://cran.r-project.org/">CRAN</a>.</li>
<li><strong>WinBUGS (>= 1.4):</strong> download 
<a href="http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/contents.shtml">from here</a>, don't forget
the patch and the immortality key, the <strong>R2WinBUGS</strong> and 
<strong>coda</strong> R packages are required for using WinBUGS from within R.</li>
<li><strong>OpenBUGS:</strong> download 
<a href="http://www.openbugs.info/w/Downloads">from here</a>, it requires the
<strong>BRugs (>= 0.3-2)</strong> R package that is 
available from <a href="http://www.stats.ox.ac.uk/pub/RWin/">CRAN Extras</a>.</li>
<li><strong>JAGS (>= 3.0.0):</strong> download 
<a href="http://sourceforge.net/projects/mcmc-jags/">from here</a>,
the <strong>rjags</strong> and <strong>coda</strong> 
R packages are required for using JAGS from within R.</li>
</ul>

<p>The dclone package itself depends on the <strong>coda</strong> (>= 0.13), <strong>R2WinBUGS</strong>, and <strong>parallel</strong>
which are all available from CRAN. 
The dclone package also suggests to have the <strong>rjags (>= 3-2)</strong>, <strong>snow</strong>, 
<strong>rlecuyer</strong>, <strong>rsprng</strong>, <strong>BRugs</strong> R packages (the latter two might not be available for all platforms).</p>

<p>The <strong>rjags</strong> package is suggested for using <strong>JAGS</strong>. 
The <strong>rjags</strong> dependency
of <strong>dclone</strong> was removed so that other functionality of the package
can be used without JAGS being installed (<strong>rjags</strong> can't load without <strong>JAGS</strong>).</p>

<p>The easiest way to install the stable release and all required packages 
at once is to type this after opening R:</p>

<p><code>install.packages("dclone")</code></p>

<p>The development version can be installed as:</p>

<p><code>install.packages("dclone", repos = "http://r-forge.r-project.org")</code></p>

<h2>Specialized packages in this repository</h2>

<ul>

<li><strong>dcmle:</strong> Hierarchical Models Made Easy with Data Cloning.
S4 classes around infrastructure provided by the dclone package to make package development 
with data cloning for hierarchical models easy as a breeze.
[<a href="http://cran.r-project.org/package=dcmle">get it from CRAN</a>]</li>

<li><strong>dcexatras:</strong> extras and not fully supported functions to the dclone package.
Only developmental version available.</li>

<li><strong>pbapply:</strong> a lightweight package that adds 
progress bar to vectorized R functions ('*apply'). The implenentation can easily be added 
to functions, where showing the progress is useful for the user (e.g. bootstrap).
[<a href="http://cran.r-project.org/package=pbapply">get it from CRAN</a>]</li>

<li><strong>sharx:</strong> data sets and SAR, SARX, HSAR and HSARX 
  models as described in Solymos and Lele (in press).
  [<a href="http://cran.r-project.org/package=sharx">get it from CRAN</a>]</li>

<li><strong>ResourceSelection:</strong> Resource Selection (Probability) Functions 
for use-availability wildlife data as described in Lele and Keim 
(2006, Ecology 87, 3021–3028), and Lele (2009, J. Wildlife Management 73, 122–127).
[<a href="http://cran.r-project.org/package=ResourceSelection">get it from CRAN</a>]</li>

<li><strong>detect:</strong> Analyzing wildlife data with detection error.
The package implements models to analyze site occupancy and count data models 
with detection error.
[<a href="http://cran.r-project.org/package=detect">get it from CRAN</a>]</li>

</ul>

</body>
</html>
