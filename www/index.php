
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

<h2>Infrastructure for dclone</h2>

<p>Because dclone is an R package, installing R is absolutely necessary. 
The choice of the preferred BUGS program is up to the
user, but in order to load the dclone package, JAGS has to be installed 
so that rjags can find it.</p>

<ul>
<li><strong>R (>= 2.7.0):</strong> download from 
<a href="http://cran.r-project.org/">CRAN</a>.</li>
<li><strong>WinBUGS:</strong> download 
<a href="http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/contents.shtml">from here</a>, don't forget
the patch and the immortality key, the <strong>R2WinBUGS</strong> and 
<strong>coda</strong> R packages are required for using WinBUGS from within R.</li>
<li><strong>OpenBUGS:</strong> download 
<a href="http://www.openbugs.info/w/Downloads">from here</a>, it requires the
<strong>BRugs (>= 0.3-2)</strong> R package that is 
available from <a href="http://www.stats.ox.ac.uk/pub/RWin/">CRAN Extras</a>.</li>
<li><strong>JAGS:</strong> download 
<a href="http://sourceforge.net/projects/mcmc-jags/">from here</a>,
the <strong>rjags</strong> and <strong>coda</strong> 
R packages are required for using JAGS from within R.</li>
</ul>

<p>The dclone package itself depends on the <strong>rjags</strong>, <strong>coda</strong> (>= 0.13), <strong>R2WinBUGS</strong>, <strong>snow</strong> and <strong>rlecuyer</strong> R packages (the latter two are for parallel computations) which are all available from CRAN. 
The dclone package also suggests to have the <strong>BRugs</strong>, <strong>rsprng</strong> R packages but these might not be available for all
operating systems. 
The easiest way to install the stable release and all required packages 
at once is to type this after opening R:</p>

<p><TT>install.packages("dclone")</TT></p>

<p>The development version can be installed as:</p>

<p><TT>install.packages("dclone", repos = "http://r-forge.r-project.org")</TT></p>

<h2>Additional resources</h2>

<ul>
<li>Some tutorials are <a href="http://dcr.r-forge.r-project.org/tutorials/">here</a> (under development).</li>
</ul>

<p></p>

<h2>Specialized packages in this repository</h2>

<ul>
<li><strong>pbapply:</strong> a lightweight package that adds 
progress bar to vectorized R functions ('*apply'). The implenentation can easily be added 
to functions, where showing the progress is useful for the user (e.g. bootstrap).</li>
<li><strong>sharx:</strong> data sets and SAR, SARX, HSAR and HSARX 
  models as described in Solymos and Lele (in press).</li>
</ul>

</body>
</html>
