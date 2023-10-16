IDS=readRDS('ID_mapping_file')
meta_objects=readRDS('Meta_objects')
summary_meta_ENSEMBL=readRDS('Summary_table_for_metaAnalysis')
Meta_results=readRDS('Final_meta_analysis_results')
forest_plot_function=function(protein_id){
  selected_protein=summary_meta_ENSEMBL[protein_id,]
  selected_protein_table=data.frame(t(selected_protein[grepl('logFC|median' ,colnames(selected_protein))]),
                                    t(selected_protein[grepl('_SEM' ,colnames(selected_protein))]),t(selected_protein[grepl("adjust",colnames(selected_protein),ignore.case = T)]),t(selected_protein[grepl("num",colnames(selected_protein))]))
  colnames(selected_protein_table)<-c('Mean_logFC',"SE_logFC","p_value","num_participants")
  rownames(selected_protein_table)=c("Granata_NVT","Granata_HVT","Deshmukh_2021","Hostrup_2021","Botella_MCIT","Botella_SIT","Schild_2015","Popov_2020")
  selected_protein_table=selected_protein_table%>%na.omit()
  meta=meta_objects[[protein_id]]
  table_data=data.frame(Study=row.names(selected_protein_table),mean=c(format(round(selected_protein_table$Mean_logFC,digits = 3))),FDR=c(format(round(selected_protein_table$p_value,digits = 3))),n=format(round(selected_protein_table$num_participants,digits = 2)))
  table_data[,c(2:4)]=data.frame(lapply(table_data[,c(2:4)],as.numeric))
  table_data=table_data[!is.na(table_data$mean),]
  summary.table=data.frame(Title="Summary Effect",mean=format(round(meta$b,3)),FDR=c(format(round(meta$pval,3))),n=format(round(sum(meta$ni)),3))
  #png(paste(protein_id,'_forestplot','.png',sep=''),width = 10,height = 5,units = 'in',res = 600)
  print(viz_forest(summary_table = summary.table,study_table = table_data,type = "standard" ,meta,annotate_CI = TRUE,study_labels = table_data$Study,table_headers = c("Study","LogFC","p.value","Num. Participants"),variant = "classic",col='firebrick',text_size =3,summary_col = "steelblue4", table_layout = matrix(c(1, 1, 2, 2, 3), nrow = 1)))
  #dev.off()
}