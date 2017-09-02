# MLExpResso 0.1.6
- new functions: plot_means, plot_pvalues
- plot_means, plot_pvalues, plot_volcano, plot_methylation_path functions added to the QuickStart

# MLExpResso 0.1.6
- Added [Online Manual](https://agosiewska.github.io/MLGenSig)
- Added [Cheatsheet](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Cheatsheet/MLExpResso-cheatsheet.pdf)
- Updated [Quick start](https://github.com/geneticsMiNIng/MLGenSig/blob/master/QuickStart/QuickStart.pdf)

# MLExpResso 0.1.5
- more tests for expression and methylation (based on packages `methyAnalysis` and `edgeR`)
- adding a simple guide for the user -  `QuickStart`
- first unit tests for package
- more parameters in public functions
- new public function names (old -> new):
  * test_diff -> calculate_test
  * comparison table -> calculate_comparison_table
  * map_to_gene -> aggregate_probes
  * mehylation_path -> plot_methylation_path
  * volcano_plot -> plot_volcano
  * group_boxplot -> plot_diff_boxplot
  * visual_volcano -> plot_volcanoes
  * visual_gene -> plot_gene
  * report_generate -> generate_report
