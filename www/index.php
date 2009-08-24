<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html lang="en"><head>








  
  
  
  
  
  
  <meta content="text/html; charset=ISO-8859-1" http-equiv="content-type"><title>ÁrvoRe - Cost-Effectiveness to R</title>






  
  
  
  
  
  
  
  
  
  <meta content="Isaías V. Prestes" name="author">


  
  
  <meta content="ArvoRe package page" name="description"></head><body>






<div style="text-align: center;"><img style="width: 200px; height: 203px;" alt="árvoRe" src="imagens/arvore.png"><br>






</div>






<div style="text-align: center;"><img style="width: 296px; height: 87px;" alt="árvore title" src="imagens/ArvoReletras.png"><br>






</div>



<div style="text-align: center;"><span style="font-family: Helvetica,Arial,sans-serif;">Language</span><br>



<table style="width: 100px; text-align: left; margin-left: auto; margin-right: auto;" border="0" cellpadding="2" cellspacing="2">



  <tbody>



    <tr>



      <td style="text-align: center;"><a href="index_ptBR.html"><img style="border: 0px solid ; width: 30px; height: 30px;" alt="Versão em Português brasileiro" title="Versão em Português brasileiro" src="imagens/brasileiro.gif"></a></td>



      <td><img style="width: 30px; height: 30px;" alt="English version" title="English version" src="imagens/ingles.gif"></td>



    </tr>



  
  
  
  </tbody>
</table>



<br>



&nbsp;&nbsp;&nbsp;&nbsp;<br>





</div>



<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%;">

</p>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; font-family: Helvetica,Arial,sans-serif;"><span style="font-size: 11pt; line-height: 150%;">The&nbsp;</span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><span style="font-weight: bold; color: rgb(0, 153, 0);">ArvoRe</span></span><span style="font-size: 11pt; line-height: 150%;"> package is a Cost-effectiveness Analysis (CEA) implementation for&nbsp;</span><span style="font-weight: bold; color: rgb(0, 0, 153);">R</span><span style="font-size: 11pt; line-height: 150%;">
oriented to compute problems that involve simple decision tree models
and Markov models. It offer a graphic user interface (GUI) developed in
Tcl/Tk. This graphical interface simplifies the decision tree creation
task and its manipulation, such as, nodes addition, probability values
definition, node kind configuration, etc.<br>



<o:p></o:p></span></p>








<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; font-family: Helvetica,Arial,sans-serif;"><span style="font-size: 11pt; line-height: 150%;">Graphics,
tables and important summary statistics for the ACE are available
trough of windows, the which ones concentrate them in an unique
location. All graphics and decision tree can be exported to image files
using PNG, JPG and BMP format. Some tables with summary statistics of
an CEA problem in this package can be exported for files in the format
CSV (comma separated values) or TXT (text plain). Other important&nbsp;</span><span style="font-size: 11pt; line-height: 150%;"></span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><span style="font-weight: bold; color: rgb(0, 153, 0);">ArvoRe</span></span><span style="font-size: 11pt; line-height: 150%;">
implementation is the CEA problems solution that involve Markov chains.
These problems are solved by means of first order Monte Carlo
simulation. A model created to a Cost-effectiveness problem in the&nbsp;</span><span style="font-size: 11pt; line-height: 150%;"></span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><span style="font-weight: bold; color: rgb(0, 153, 0);">ArvoRe</span></span><span style="font-size: 11pt; line-height: 150%;">
can be saved in a file with extension ARV. The main user preferences
settings - as kind of tree to be exhibited, definitions that should be
also exhibited are conserved in this extension file ARV.<br>



</span></p>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; font-weight: bold; color: rgb(0, 153, 0); font-family: Helvetica,Arial,sans-serif;">REQUIREMENTS</p>






<ul style="margin-left: 40px; font-family: Helvetica,Arial,sans-serif;">






  <li><span style="font-weight: bold; color: rgb(0, 0, 153);">R</span>&nbsp;2.6.2 or newer (now compatible with R using Tcl/Tk 8.5)</li>






</ul>






<div style="margin-left: 120px;">

<p class="MsoNormal" style="text-align: justify; text-indent: 35.45pt; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%; font-family: Arial;">To run&nbsp;</span><span style="font-size: 11pt; line-height: 150%;"></span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><span style="font-weight: bold; color: rgb(0, 153, 0);">ArvoRe</span></span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"> is necessary&nbsp;</span><span style="font-weight: bold; color: rgb(0, 0, 153); font-family: Arial;">R</span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"> version 2.6.2 or superior version.</span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><br>



<o:p></o:p></span></p>







</div>






<ul style="margin-left: 40px; font-family: Helvetica,Arial,sans-serif;">






  <li>Packages <span style="font-weight: bold;">abind</span>, <span style="font-weight: bold;">grid</span>, <span style="font-weight: bold;">gplots, tcltk </span>and <span style="font-weight: bold;">tcltk2</span></li>






</ul>






<div style="margin-left: 120px;">

<p class="MsoNormal" style="text-align: justify; text-indent: 35.45pt; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><span style="font-weight: bold; color: rgb(0, 153, 0);">ArvoRe</span></span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;">&nbsp;requires&nbsp;</span><span style="font-weight: bold; color: rgb(0, 0, 153); font-family: Arial;">R</span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"> version 2.6.2 or newer and some packages: abind, grid, gplots tcltk and tcltk2. This
packages must be installed in your system. These packages can be
downloaded from Comprehensive&nbsp;</span><span style="font-weight: bold; color: rgb(0, 0, 153); font-family: Arial;">R</span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"> Archive Network, CRAN&nbsp;</span><span style="font-size: 11pt; font-family: Arial;"></span><span style="font-size: 11pt; font-family: Arial;"><a href="http://www.r-project.org/">http://www.R-project.org</a>
.&nbsp;</span></p>







</div><br>
<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; color: rgb(0, 153, 0); font-weight: bold; font-family: Helvetica,Arial,sans-serif;">DEVELOPMENT</p>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;"><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"></span><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><span style="font-weight: bold; color: rgb(0, 153, 0);">ArvoRe</span></span>&nbsp;package
is developed through a SVN's System in R-Forge. Anybody can contribute
with improvement and/or development of code. Visit&nbsp;<a href="http://r-forge.r-project.org/projects/arvore" target="_blank">ArvoRe project page</a>.</p>




<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;">A more recent and unstable copy of this package can be directly installed in the R using the command </p>




<p class="MsoNormal" style="text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif; text-align: center;"><strong><code>install.packages("<i>arvore</i>",repos="http://R-Forge.R-project.org")</code></strong></p>




<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;"><strong><code></code></strong>Development versions, the "latest snapshot", can be acquired below.</p>




<p class="MsoNormal" style="text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif; text-align: center;"><a href="http://r-forge.r-project.org/snapshots.php?group_id=211">scm-latest.tar.gz</a></p>




<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;"> </p>
<br>
<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; color: rgb(0, 153, 0); font-weight: bold; font-family: Helvetica,Arial,sans-serif;">REFERENCE / HELP</p>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;"><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"></span>You can read the paper "<a href="file:///C:/xampp/htdocs/arvore/outros/TCCarticle.pdf" target="_blank">Implementação de uma interface gráfica para Análise de Custo Efetividade no R</a>" to solve instalation problems and know more about Cost-Effectiveness Analysis.</p>
<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;"></p>
<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; color: rgb(0, 153, 0); font-weight: bold; font-family: Helvetica,Arial,sans-serif;">DOWNLOAD</p>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;">Binary (Windows)</p>






<ul style="font-family: Helvetica,Arial,sans-serif;">






  
  
  
  
  
  
  <ul>






    
    
    
    
    
    
    <ul>






      <li><a href="binario/arvoRe_0.1.7.zip">ÁrvoRe versão alpha-0.1.7 (instável)</a></li>






    
    
    
    
    
    
    </ul>






  
  
  
  
  
  
  </ul>






</ul>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 40px; font-family: Helvetica,Arial,sans-serif;">Source code</p>






<ul style="font-family: Helvetica,Arial,sans-serif;">






  
  
  
  
  
  
  <ul>






    
    
    
    
    
    
    <ul>






      <li><a href="source/arvoRe_0.1.7.tar.gz">ÁrvoRe versão alpha-0.1.7 (instável)</a></li>






    
    
    
    
    
    
    </ul>






  
  
  
  
  
  
  </ul>






</ul>






<br style="font-family: Helvetica,Arial,sans-serif;">






<div style="margin-left: 40px; font-family: Helvetica,Arial,sans-serif;"><span style="font-weight: bold; color: rgb(0, 153, 0);">CONTACT</span><br>








<p class="MsoNormal" style="text-align: justify; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%;">Isaías Prestes,
Suzi Camey<o:p></o:p></span></p>








<p class="MsoNormal" style="text-align: justify; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%;">Departamento de
Estatística<o:p></o:p></span></p>








<p class="MsoNormal" style="text-align: justify; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%;">Universidade
Federal do Rio Grande do Sul<o:p></o:p></span></p>








<p class="MsoNormal" style="text-align: justify; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%;">Rio Grande do Sul,
Porto Alegre<o:p></o:p></span></p>








<p class="MsoNormal" style="text-align: justify; line-height: 150%;"><span style="font-size: 11pt; line-height: 150%;">Brasil Av. Bento
Gonçalves, 9500<o:p></o:p></span></p>








<span style="font-size: 11pt;">E-mail:
<a href="mailto:isaias.prestes@gmail.com">isaias.prestes@gmail.com</a>,
<a href="mailto:camey@ufrgs.br">camey@ufrgs.br</a><br>




<br>




<br>




</span>
<div style="text-align: center;"><span style="font-size: 11pt;"><a href="http://r-forge.r-project.org"><img style="border: 0px solid ; width: 210px; height: 54px;" alt="R-Forge logo" src="imagens/logo_Rforge.png"></a></span><span style="font-size: 11pt;"></span></div>




</div>






<br style="font-family: Helvetica,Arial,sans-serif;">






<span style="font-family: Helvetica,Arial,sans-serif;"><span style="font-family: Helvetica,Arial,sans-serif;"><br>





<br>






</span></span>
<hr style="width: 100%; height: 2px;">
<div style="text-align: center;"><span style="font-family: Helvetica,Arial,sans-serif;"><span style="font-family: Helvetica,Arial,sans-serif;"></span></span><span style="font-family: Helvetica,Arial,sans-serif;"><span style="font-family: Helvetica,Arial,sans-serif;">August 04, 2008 09:38:28 PM </span></span></div>






<div style="text-align: center;"><span style="font-family: Helvetica,Arial,sans-serif;"></span></div>






<p class="MsoNormal" style="text-align: justify; text-indent: 35.4pt; line-height: 150%; margin-left: 80px;"><span style="font-size: 11pt; line-height: 150%; font-family: Arial;"><o:p></o:p></span></p>







</body></html>