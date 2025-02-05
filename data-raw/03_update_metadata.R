#  TITLE: fun_update_metadata.R
#  DESCRIPTION: Adds EJ metric shapefiles
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2025-02-05
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
# -----------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(xml2)
library(XML)

# Set variables ---------------------------------------------------------------
epagrants <- 'CE00A00967'
usethis::use_data(epagrants, overwrite=TRUE)

ejmap_year = format(Sys.Date(), '%Y')
usethis::use_data(ejmap_year, overwrite = TRUE)

# Read in XML metadata --------------------------------------------------------
input_xml <- xml2::read_xml('data-raw/EJMETRICS_2023_NBEP2023.shp.xml')

# Update XML metadata --------------------------------------------------------
# * Update summary ----
node_idPurp <- xml_find_all(input_xml, '//idPurp')
text_idPurp <- paste0('Environmental justice indicators and priority areas in the Narragansett Bay region at the U.S. Census block group scale. Scores were calculated using the Narragansett Bay Estuary Program EJMAP tool (NBEP ', 
                      ejmap_year, 
                      '). This dataset is intended for general planning, graphic display, and GIS analysis.')
xml_text(node_idPurp) <- text_idPurp

# * Update description ----
node_idAbs <- xml_find_all(input_xml, '//idAbs')
text_idAbs <- paste0('Environmental justice indicators and priority areas in the Narragansett Bay region at the U.S. Census block group scale. Scores were calculated using the Narragansett Bay Estuary Program EJMAP tool (NBEP ', 
                     ejmap_year, 
                     '). This dataset is intended for general planning, graphic display, and GIS analysis.')
xml_text(node_idAbs) <- text_idAbs

# * Add fields ----
node_field_parent <- xml_find_all(input_xml, '//detailed')
list_fields <- c('P_SOCVUL', 'N_SOCVUL', 'P_HEALTH', 'N_HEALTH', 'P_ENVBUR',
                 'N_ENVBUR', 'P_CLIMATE', 'N_CLIMATE', 'P_SCORE', 'N_SCORE',
                 'P_EJAREA', 'N_EJAREA')

for (field in list_fields){
  # Define variables
  field_type = str_sub(field, 1, 1)
  field_type_sub = list(P = 'State', N = 'Regional')
  
  field_name = str_sub(field, 3, -1)
  field_name_sub = list(
    SOCVUL = 'social vulnerability score',
    HEALTH = 'health score',
    ENVBUR = 'environmental burden score',
    CLIMATE = 'climate score',
    SCORE = 'environmental justice score',
    EJAREA = 'environmental justice area'
  )
  
  field_definition = paste(field_type_sub[field_type], 
                           field_name_sub[field_name])
  
  # Create node with field info
  node_field <- paste0(
    '<attr>
      <attrlabl>', field, '</attrlabl>
      <attrdef>', field_definition, '</attrdef>
      <attrdefs>Narragansett Bay Estuary Program</attrdefs>
      <attrdomv>'
  )
  
  if (field_name == 'EJAREA'){
    node_field <- paste0(node_field,
      '<edom>
        <edomv>Yes</edomv>
        <edomvd>Block group is an environmental justice area</edomvd>
        <edomvds>Narragansett Bay Estuary Program</edomvds>
      </edom>
      <edom>
        <edomv>No</edomv>
        <edomvd>Block group is not an environmental justice area</edomvd>
        <edomvds>Narragansett Bay Estuary Program</edomvds>
      </edom>
      <edom>
        <edomv>No Data</edomv>
        <edomvd>Insufficient data to determine if block group is an environmental justice area</edomvd>
        <edomvds>Narragansett Bay Estuary Program</edomvds>
      </edom>'
      )
  } else {
    node_field <- paste0(node_field, 
      '<rdom><rdommin>0</rdommin><rdommax>100</rdommax></rdom>'
      )
  }
  node_field <- paste0(node_field, '</attrdomv></attr>')
  
  # Add child node
  xml_add_child(node_field_parent, read_xml(node_field))
}

# Add (blank) process step ----
node_process_parent <- xml_find_all(input_xml, '//dataLineage')
node_process <- read_xml(
  '<prcStep>
    <stepDesc>EJmap (https://github.com/nbep/ejmap) was used to produce an environmental justice map.</stepDesc>
    <stepDateTm>2023-09-26T00:00:00</stepDateTm>
    <stepProc>
      <rpOrgName>Narragansett Bay Estuary Program</rpOrgName>
      <rpCntInfo>
        <cntAddress addressType="both">
          <delPoint>235 Promenade Street, Suite 393</delPoint>
          <city>Providence</city>
          <adminArea>Rhode Island</adminArea>
          <postCode>02908</postCode>
          <eMailAdd>info@nbep.org</eMailAdd>
          <country>US</country>
				</cntAddress>
				<cntPhone>
					<voiceNum tddtty="">(401) 633-0550</voiceNum>
				</cntPhone>
			</rpCntInfo>
		</stepProc>
  </prcStep>'
)

xml_add_child(node_process_parent, node_process)

# Tidy data ----
# Remove display names
node_displayName <- xml_find_all(input_xml, '//displayName')
xml_remove(node_displayName)

# Remove namespace
xml2::xml_ns_strip(input_xml)

# Update field names
node_geoid <- xml2::xml_find_all(input_xml, '//attrlabl[text()="GEOID"]')
xml2::xml_text(node_geoid) <- 'BlockGroup'

# Save XML --------------------------------------------------------------------
write_xml(input_xml, 'inst/extdata/metadata.xml')
