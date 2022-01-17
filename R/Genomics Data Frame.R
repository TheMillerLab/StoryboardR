#' Create a Data Frame of Genomics
#' @description
#' `genomics_df()` creates a table of genomic information
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame
#' @export
#'
genomics_df <- function(data){
  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # prepare data frame
  ##########################################################################################################################
  # Replace values with strings
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 1, "Primary Cutaneous")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 2, "Metastases")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 3, "MCCUP")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 4, "Local Recurrence")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 5, "Liquid Biopsy")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 98, "Unknown")

  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 1, "BWH OncoPanel")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 2, "MGH SNaPshot")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 3, "MSK Impact")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 4, "Foundation One CDx")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 5, "Tempus xT Gene Panel")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 6, "Guardant 360")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 99, "Other")

  ##########################################################################################################################
  # Convert Dates
  ##########################################################################################################################
  dt$genomics_date <- as.Date(
    x = dt$genomics_date,
    format = "%m/%d/%y",
    origin = "1900-01-01"
  )

  ##########################################################################################################################
  # Create DFs of gene variants
  ##########################################################################################################################
  variants.gene <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           contains("variant_gene"))

  variants.gene.g <- variants.gene %>%
    gather(key = "variable",
           value = "gene",
           -record_id,
           -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("gene")

  ##########################################################################################################################
  # Drop cfDNA from the variable column
  ##########################################################################################################################

  variants.gene.g.1 <- variants.gene.g %>%
    filter(!str_detect(string = variants.gene.g$variable,
                       pattern = regex("cfdna", ignore_case = TRUE)))



  # Separate to isolate the number
  variants.gene.g.split.pre <- variants.gene.g.1 %>%
    separate(variable, c("text","number"),"variant_gene_")

  variants.gene.g.split <- variants.gene.g.split.pre[,-4] ## let's get rid of this worthless column 3 here

  # build table for variant protein
  variants.protein <- dt %>%
    select(record_id, lesion_tag_genomics, genomics_tissue_type, contains("variant_protein"))

  variants.protein.g <- variants.protein %>%
    gather(key = "variable",
           value = "protein",
           -record_id,
           -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("protein")

  # Seperate to isolate the protein
  variants.protein.g.split.pre <- variants.protein.g %>%
    separate(variable, c("text","number"),"variant_protein_")

  variants.protein.g.split <- variants.protein.g.split.pre[,-4]

  variants.nucleotide <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           contains("variant_nucleotide"))

  variants.nucleotide.g <- variants.nucleotide %>%
    gather(key = "variable",
           value = "nucleotide",
           -record_id, -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("nucleotide")

  # Seperate to isolate the nucleotide
  variants.nucleotide.g.split.pre <- variants.nucleotide.g %>%
    separate(variable,
             c("text","number"),
             "variant_nucleotide_")

  variants.nucleotide.g.split <- variants.nucleotide.g.split.pre[,-4]

  ##########################################################################################################################
  # Let's create a table of metadata
  ##########################################################################################################################

  genomics.meta <- dt %>%
    dplyr::select(record_id,
                  lesion_tag_genomics,
                  genomics_tissue_type,
                  genomics_platform) %>%
    drop_na(lesion_tag_genomics)

  ## Now let's combine these tables

  snv <- left_join(variants.gene.g.split,
                   variants.nucleotide.g.split,
                   by =c("record_id" = "record_id",
                         "lesion_tag_genomics" = "lesion_tag_genomics",
                         "genomics_tissue_type" = "genomics_tissue_type",
                         "number" = "number"))

  # Now let's combine these tables
  snv <- left_join(snv,
                   variants.protein.g.split,
                   by =c("record_id" = "record_id",
                         "lesion_tag_genomics" = "lesion_tag_genomics",
                         "genomics_tissue_type" = "genomics_tissue_type",
                         "number" = "number"))

  # arrange snv by gene
  snv <- snv %>%
    arrange(gene)

  # Delete everything before the period in the nucleotide column
  snv$nucleotide  <- gsub("^.*\\.",
                          "",
                          snv$nucleotide)

  # Delete everything before the period in the protein column
  snv$protein  <- gsub("^.*\\.",
                       "",
                       snv$protein)

  #Drop the number column
  snv <- snv %>%
    select(-contains("number"))


  genomics.var <- dt %>%
    select(record_id,
           contains("variant_gene"))

  genomics.var.g <- genomics.var %>%
    gather(key = "variable",
           value = "gene",
           -record_id) %>%
    drop_na("gene")

  genomics.var.num <- genomics.var.g %>%
    count(record_id, gene) %>%
    count(gene, sort = TRUE,
          name="Number_of_alterations")

  # Count number of genes with ONLY CNVs
  cnv <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           contains("cnv_gene"))

  cnv.g <- cnv %>%
    gather(key = "variable",
           value = "gene",
           -record_id,
           -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("gene")

  #Add a vector for "Yes" to help ID which genes had CNVs as well
  cnv.g$cnv_yn <- "Yes"
  # drop variable
  cnv.g <- cnv.g[,-4]


  # full join CNV to SNV
  genomics.full_join <- full_join(snv, cnv.g,
                                  by = c("record_id" = "record_id",
                                         "lesion_tag_genomics" = "lesion_tag_genomics",
                                         "genomics_tissue_type" = "genomics_tissue_type",
                                         "gene" = "gene")) %>%
    dplyr::arrange(gene)  # were using a full join b/c we want to be able to see rows for CNV only

  # Replace NA with "No" for cnv_yn
  genomics.full_join$cnv_yn <- replace(genomics.full_join$cnv_yn,
                                       is.na(genomics.full_join$cnv_yn),
                                       "No")


  genomics.full_join.meta <- full_join(genomics.full_join,
                                       genomics.meta)


  genomics.full_join.select <- genomics.full_join.meta %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           genomics_platform,
           gene)

  genomics.date <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_date) %>%
    drop_na(lesion_tag_genomics)

  genomics.full_join.select.date <- left_join(genomics.full_join.select,
                                              genomics.date,
                                              by = c("record_id",
                                                     "lesion_tag_genomics"))

  genomics.pivot.wider <- genomics.full_join.select.date %>%
    group_by(record_id,
             lesion_tag_genomics,
             genomics_date) %>%
    mutate(genomic_alterations = paste0(gene,
                                        collapse = ", ")) %>%
    ungroup()

  genomics.pivot.wider$genomic_alterations[genomics.pivot.wider$genomic_alterations == "NA"] <- "None Detected"

  genomics.pivot.wider.slice <- genomics.pivot.wider %>%
    group_by(record_id,
             lesion_tag_genomics,
             genomics_date) %>%
    slice_head() %>%
    arrange(genomics_date) %>%
    ungroup() %>%
    select(-gene)

  # Format Lesion Tag
  genomics.df.1 <- genomics.pivot.wider.slice
  genomics.df.1$lesion_tag_genomics  <- stringr::str_trim(string = genomics.df.1$lesion_tag_genomics)
  genomics.df.1$lesion_tag_genomics  <- stringr::str_to_title(string = genomics.df.1$lesion_tag_genomics)


  ##########################################################################################################################
  # TMB
  ##########################################################################################################################
  # Select those with tmb
  tmb <- dt %>% select(record_id, tmb, genomics_date) %>% drop_na()
  # select those with tmb_abs
  tmb.1 <- dt %>% select(record_id, tmb_abs, genomics_date) %>% drop_na()
  tmb.2 <- tmb.1 %>% rename(`tmb` = tmb_abs)
  # combine the above two
  tmb.3 <- rbind(tmb, tmb.2)

  ##########################################################################################################################
  # Combine TMB with genomics df
  ##########################################################################################################################
  genomics.df.2 <- left_join(genomics.df.1, tmb.3, by = c("record_id", "genomics_date"))

  ##########################################################################################################################
  # Rename Variables
  ##########################################################################################################################
  genomics.df.3 <- genomics.df.2 %>%
    rename(`Lesion Tag` = lesion_tag_genomics,
           `Tissue` = genomics_tissue_type,
           `Platform` = genomics_platform,
           `Date` = genomics_date,
           `TMB` = tmb,
           `Genetic Alterations`  = genomic_alterations)

  genomics.df.3 <- genomics.df.3 %>% dplyr::arrange(Date)


  return(genomics.df.3)
}
