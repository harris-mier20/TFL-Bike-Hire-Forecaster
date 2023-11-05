library(sf)

#coordinates for vertices for WC1
wc1_matrix_simplified <- matrix(c(
  -0.1298618, 51.5159003,
  -0.1382732, 51.5253532,
  -0.1223087, 51.5305862,
  -0.1115799, 51.5316007,
  -0.1132107, 51.5215616,
  -0.1298618, 51.5159003
), ncol = 2, byrow = TRUE)


# Create the sf polygon for wc1 and convert to simple features for display on map
wc1_sf <- st_polygon(list(wc1_matrix))
wc1_sf <- st_sfc(wc1_sf)

#coordinates for wC2
wc2_matrix <- matrix(c(
  -0.1298511, 51.5158402,
  -0.1298618, 51.5151592,
  -0.1296204, 51.5144915,
  -0.1291752, 51.5132029,
  -0.1345396, 51.5099444,
  -0.1283169, 51.5072199,
  -0.1227379, 51.5063385,
  -0.1175451, 51.5100245,
  -0.1115370, 51.5109861,
  -0.1112366, 51.5138705,
  -0.1116496, 51.5182470,
  -0.1207410, 51.5171012,
  -0.1298518, 51.5158398,
  -0.1298511, 51.5158402
), ncol = 2, byrow = TRUE)

# Create the sf polygon for wc2 and convert to simple features for display on map
wc2_sf <- st_polygon(list(wc2_matrix))
wc2_sf <- st_sfc(wc2_sf)

#coordinates for EC1
ec1_matrix <- matrix(c(
  -0.1113760, 51.5316141,
  -0.1092276, 51.5276444,
  -0.1131892, 51.5215591,
  -0.1116309, 51.5182770,
  -0.1052713, 51.5172088,
  -0.1050997, 51.5177964,
  -0.0974178, 51.5203867,
  -0.0973749, 51.5215883,
  -0.0872469, 51.5220422,
  -0.0871825, 51.5257537,
  -0.0884485, 51.5272623,
  -0.1062047, 51.5319278,
  -0.1113760, 51.5316141
), ncol = 2, byrow = TRUE)

# Create the sf polygon for ec1 and convert to simple features for display on map
ec1_sf <- st_polygon(list(ec1_matrix))
ec1_sf <- st_sfc(ec1_sf)

#coordinates for EC2
ec2_matrix <- matrix(c(
  -0.0871074, 51.5257638,
  -0.0777411, 51.5271021,
  -0.0770116, 51.5237645,
  -0.0787497, 51.5210008,
  -0.0808096, 51.5173691,
  -0.0841999, 51.5136035,
  -0.0887489, 51.5133364,
  -0.1051909, 51.5171721,
  -0.1050702, 51.5177713,
  -0.0973856, 51.5203716,
  -0.0973535, 51.5215750,
  -0.0872147, 51.5220172,
  -0.0871047, 51.5257604,
  -0.0871074, 51.5257638
), ncol = 2, byrow = TRUE)

# Create the sf polygon for ec2 and convert to simple features for display on map
ec2_sf <- st_polygon(list(ec2_matrix))
ec2_sf <- st_sfc(ec2_sf)

#coordinates for EC3
ec3_matrix <- matrix(c(
  -0.0793505, 51.5190782,
  -0.0709605, 51.5155130,
  -0.0741148, 51.5070063,
  -0.0820971, 51.5084754,
  -0.0874615, 51.5090363,
  -0.0864744, 51.5106656,
  -0.0887382, 51.5133197,
  -0.0841784, 51.5135868,
  -0.0824082, 51.5154329,
  -0.0806379, 51.5173490,
  -0.0796562, 51.5192084,
  -0.0793558, 51.5190748,
  -0.0793505, 51.5190782
), ncol = 2, byrow = TRUE)

# Create the sf polygon for ec3 and convert to simple features for display on map
ec3_sf <- st_polygon(list(ec3_matrix))
ec3_sf <- st_sfc(ec3_sf)

#coordinates for ec4
ec4_matrix <- matrix(c(
  -0.1114780, 51.5109594,
  -0.1112225, 51.5138668,
  -0.1114056, 51.5160572,
  -0.1116201, 51.5182453,
  -0.1084659, 51.5177263,
  -0.1052821, 51.5171421,
  -0.0969753, 51.5152309,
  -0.0887690, 51.5133264,
  -0.0864972, 51.5106614,
  -0.0869974, 51.5098543,
  -0.0874883, 51.5090429,
  -0.0937057, 51.5097708,
  -0.1003361, 51.5108792,
  -0.1114807, 51.5109544,
  -0.1114780, 51.5109594
), ncol = 2, byrow = TRUE)

# Create the sf polygon for ec4 and convert to simple features for display on map
ec4_sf <- st_polygon(list(ec4_matrix))
ec4_sf <- st_sfc(ec4_sf)
