library(RecordLinkage)
library(dplyr)
library(tictoc)
source("R/compare_records.R")
source("R/create_breaklist.R")
source("R/compute_string_distance.R")
source("R/one_hot_encode_factor.R")
source("R/estimate_links_fabl.R")
source("R/estimate_links_vabl_svabl.R")
source("R/estimate_links_no_reject.R")
source("R/estimate_links_with_reject.R")
source("R/estimate_links_resolve_1to1.R")
source("R/estimate_links.R")
source("R/evaluate_links.R")
source("R/hash_field.R")
source("R/possible_patterns_ohe.R")
source("R/fs_to_ohe.R")
source("R/sei.R")
source("R/hash_comparisons.R")
source("R/svabl.R")
source("R/svabl_setup.R")
source("R/vabl_svabl_compute_chunks.R")
source("R/svabl_compute_m_u_p.R")
source("R/svabl_compute_batch_counts.R")
source("R/svabl_compute_K.R")
source("R/svabl_update_ab.R")
source("R/vabl_svabl_compute_elbo.R")
source("R/vabl.R")
source("R/vabl_compute_m_u_p.R")
source("R/vabl_setup.R")
source("R/vabl_compute_phi_C.R")
source("R/vabl_compute_ABZ.R")

# Load the dataset and add a unique ID
data <- RecordLinkage::RLdata10000 %>%
  mutate(unique_id = RecordLinkage::identity.RLdata10000)

# Calculate 10% duplicates
duplicates <- nrow(data) * .1

# Identify duplicated records by unique_id
duplicated_ids <- data %>%
  filter(duplicated(unique_id)) %>%
  select(unique_id) %>%
  pull()

# Extract duplicated records and assign row numbers
duplicated_records <- data %>%
  filter(unique_id %in% duplicated_ids) %>%
  arrange(unique_id) %>%
  mutate(rn = row_number())

# Split duplicated records into two sets
duplicated_1 <- duplicated_records %>%
  filter(rn %% 2 == 0)

duplicated_2 <- duplicated_records %>%
  filter(rn %% 2 == 1)

# Extract non-duplicated records and assign row numbers
non_duplicated_records <- data %>%
  filter(!(unique_id %in% duplicated_ids)) %>%
  mutate(rn = row_number())

# Split non-duplicated records into two sets
`non_duplicated_1` <- non_duplicated_records %>%
  filter(rn %% 2 == 0)

non_duplicated_2 <- non_duplicated_records %>%
  filter(rn %% 2 == 1)

# Combine both sets into two separate dataframes
df1 <- rbind(duplicated_1, non_duplicated_1)
df2 <- rbind(duplicated_2, non_duplicated_2)

# Count number of rows in df1 and df2
n1 <- nrow(df1)
n2 <- nrow(df2)


fields <- c(1, 3, 5, 6, 7)
types <- c("lv", "lv", "bi", "bi", "bi")
breaks <- c(0, .25, .5)

start <- tic()
cd <- compare_records(df1, df2, fields = fields, types = types,
                      breaks = breaks)
compare_time <- unname(toc(quiet = T)$toc - start)
print(compare_time)


start <- tic()
hash <- hash_comparisons(cd, all_patterns = F)
hash_time <- unname(toc(quiet = T)$toc - start)
print(hash_time)

out_vabl <- vabl(hash)

out_svabl <- svabl(hash, B = 100, k = 1, tau = 1)

results_vabl <- estimate_links(out_vabl, hash)

results_svabl <- estimate_links(out_svabl, hash)

Z_true <- rep(0, nrow(df1))
Z_true[1:duplicates] <- 1:duplicates

eval_vabl <- evaluate_links(results_vabl$Z_hat, Z_true, n1)
print(eval_vabl)

eval_svabl <- evaluate_links(results_svabl$Z_hat, Z_true, n1)
print(eval_svabl)

