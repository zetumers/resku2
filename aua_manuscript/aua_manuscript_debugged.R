#bug: it seems like the number of operations is not consistently reported.
#when looking at the wide df, the number is 548.
#when looking at the narrow df, the number is 502. So about 9%
#the surgery data is not reported. WE MUST GET TO THE BOTTOM OF THIS!

resku %>%
  filter(date_surgery_v2 != "") %>%
  select(record_id, date_surgery_v2, contains("stone_op")) %>%
  group_by(record_id) %>%
  do(.[1,]) -> testa
testa$sum <- rowSums(testa[,3:length(testa)])
testa %>%
  mutate(any = (sum > 0)) %>%
  filter(any == 1) -> testb

print(length(testa$record_id))

print(length(unique(testa$record_id)))

print(length(testb$record_id))

print(length(unique(testb$record_id)))

#means that there are roughly 641 patients who get some sort of "1" in the list of
#ops

#change aua_op_id to debug!
aua_op_id <- function(long_df){
  long_df %>%
    #gives me only the "stone_op" variables
    filter(grepl("stone_op___",variable)) %>%
    #give me only the surgeries that actually happened
    filter(value == 1) %>%
    #replace the value with the number of the operation
    mutate(value = aua_pull_nums(variable, "stone_op")) %>%
    #get rid of the actual stone op part
    select(-variable) %>%
    #rename the value as the original variable
    dplyr::rename(stone_op = value) %>%
    arrange(record_id, order) -> long_df
  return(long_df)
}

aua_op_id(resku_df) -> testc

print(length(unique(testc$record_id)))

#turns out everything is accounted for! there are 502 unique ids that are good to
#go.
#There are about 548 records that had surgeries but no recording of them.
#There are about 737 surgeries.

#TEST 2:
#Now we need to see if there are any data on the type of operation based on the
#post_op fu appointment

resku %>%
  filter(date_fu_postop != "") %>%
  select(record_id, contains("stone_postop")) -> testa2
testa2$sum <- rowSums(testa2[,2:length(testa2)])
testa2 %>%
  mutate(any = (sum > 0)) %>%
  filter(any == 1) -> testb2

print(length(testa2$record_id))
print(length(unique(testa2$record_id)))
print(length(testb2$record_id))
print(length(unique(testb2$record_id)))

####See if we can get patients who got a pnl


resku_df %>%
  filter(variable == "pnl_access_person") -> pnl_access_person_df

resku_df %>%
  filter(variable == "pnl_punct_guide") -> pnl_punct_guide_df

#none of these things add any too information.

#GET A LIST OF ACTUAL DATES AND TIMES
#TO DO THIS, YOU NEED TO HAVE ONE OF THE FILES
#WITH THE MRNS and THE REAL DATES

filename <<- "../data/resku_debug.csv"
aua_pull_resku()

resku %>%
  filter(date_surgery_v2 != "") %>%
  select(record_id, date_surgery_v2, contains("stone_op")) %>%
  group_by(record_id) %>%
  do(.[1,]) -> testa
testa$sum <- rowSums(testa[,3:length(testa)])
testa %>%
  mutate(any = (sum > 0)) %>%
  filter(any == 1) -> testb

#now we need to add some hippa information, so those running this file later will
#likely not have access to this.
mrns <- read.csv("../data/resku_debug.csv")
mrns %>%
  filter(!is.na(mrn)) %>%
  select(record_id, mrn) -> mrns2

#testc is all patients with a first operation that does not have surgical id data
#reported.
testa %>%
  filter(!(record_id %in% testb$record_id)) %>%
  left_join(mrns2, by = "record_id") %>%
  select(record_id, mrn, date_surgery_v2) -> lookup_list

write.csv(lookup_list,"lookup_list.csv")