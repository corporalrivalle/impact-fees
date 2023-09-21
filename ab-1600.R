
library(pdftools)
library(tidyverse)

# downloading santa rosa's AB 1600 reports---only needs to be run once
years <- c(2021, 2005:2014, 2020:2015)
adid <- c(4446, 4423:4408)

for (i in 1:length(years)) {
  download.file(
    url = paste0('https://www.srcity.org/ArchiveCenter/ViewFile/Item/', adid[i]),
    destfile = paste0('fees/', years[i], '.pdf'),
    mode = 'wb'
  )
}


all_years <- NULL

for (year in 2021:2015) {
  text <- pdf_text(pdf = paste0('fees/', year, '.pdf'))
  final <- NULL

  first_page <- min(which(str_detect(text, 'IFAS #')))  # only the data pages have this string

  for (page in first_page:length(text)) {
    fulltext <- text[[page]] %>%
      str_split_1(pattern = '\n') %>%  # split at line breaks
      str_subset(pattern = '.+') %>%  # remove any empty lines
      str_squish()

    fulltext <- fulltext[!fulltext == 'CITY OF SANTA ROSA']  # get rid of heading

    #fund <- fulltext[min(which(str_detect(fulltext, 'FEE')))]
    fund <- fulltext[1]

    index <- which(str_detect(fulltext, 'Ending'))  # only parse till ending balance

    tabletext <- fulltext[3:index] %>%
      str_replace('$', '\n') %>%
      # space preceded by alpha and followed by alpha or open parenthesis and alpha
      str_replace_all('(?<=[:alpha:])\\s(?=[:alpha:]|\\([:alpha:])', '_')

    table <- read.table(text = tabletext, col.names = c('id', 'value'), fill = TRUE) %>%
      mutate(fund = fund)

    if (page < 8) {
      final <- table
    } else {
      final <- bind_rows(final, table)
    }
  }

  clean <- final %>%
    filter(!str_detect(fund, 'Page [2-9]')) %>%  # eliminate duplicate pages
    mutate(
      type = case_when(
        id %in% c('Beginning_Fund_Balance', 'Beginning_Balance') ~ 'start',
        id == 'Fees_collected'                                   ~ 'collect',
        id == 'Interest_Earnings'                                ~ 'interest',
        id == 'Change_in_Fair_Value'                             ~ 'gain_loss',
        id %in% c('Annual_Expenditures',
                  'Expenditure_to_Public_Works',
                  'Public_Works_expenditure',
                  'Annual_Expenditures_(Transfers_Out)')         ~ 'expenditure',
        id %in% c('Ending_Fund_Balance', 'Ending_Balance')       ~ 'end',
        id %in% c('Transfer_to_General_Fund',
                  'Transfer_out_to_CDBG-DR',
                  'Transfer_out_to_CDBG',
                  'Transfer_to_Storm_Creek',
                  'Transfer_out_to_General_Fund',
                  'Transfer_to_Storm')                           ~ 'transfer_out',
        id %in% c('Transfer_In_from_Southeast_Area_Dev',
                  'Transfer_In_from_Southeast_Area_Development',
                  'Transfer_In_from_Storm_Creek',
                  'Transfer_In_from_General_Fund')               ~ 'transfer_in',
        id == 'Loan_Repayments'                                  ~ 'amortization',
        id %in% c('Cost_Reimbursement', 'Cost_reimbursement')      ~ 'reimbursement',
        id == 'Reclass_to_Retained_Earnings_(Per_Financial_Reporting)' ~ 'reclass'
      ),

      fund_clean = case_when(
        str_detect(fund, 'CAPITAL FACILITIES') ~ 'capital',
        str_detect(fund, 'PARK ACQUISITION')   ~ 'park',
        str_detect(fund, 'SOUTHEAST')          ~ 'southeast',
        str_detect(fund, 'SOUTHWEST')          ~ 'southwest',
        str_detect(fund, 'HOUSING ALLOCATION') ~ 'allocation',
        str_detect(fund, 'WASTEWATER')         ~ 'sewer',
        str_detect(fund, '^WATER')             ~ 'water',
        str_detect(fund, 'PUBLIC FACILITIES')  ~ 'public',
        str_detect(fund, 'TRAFFIC')            ~ 'traffic'
      ),

      value_clean = str_replace(value, '\\-', '\\('),

      sign = case_when(
        type %in% c('start', 'collect', 'interest', 'gain_loss',
                    'end', 'transfer_in', 'amortization', 'reimbursement') ~ 1,
        type %in% c('expenditure', 'transfer_out') ~ -1,
        type == 'reclass' ~ 0
      ),

      sign = if_else(str_detect(value_clean, '\\('),
                     sign * -1, sign),

      money = parse_number(value_clean, na = '(') * sign,

      year = year
    )

  if (year >= 2021) {
    all_years <- clean
  } else {
    all_years <- bind_rows(all_years, clean)
  }
}


# visualizing change in funds from 2014-2021
totals <- all_years %>%
  group_by(type, fund_clean, year) %>%
  summarize(total = sum(money, na.rm = TRUE))

ggplot(totals %>% filter(type == 'expenditure'),
       aes(x = year, y = total * -1, color = fund_clean, group = fund_clean)) +
  geom_line() +
  geom_point()


# checking that math is correct---discrepancies are ~$1000 or less so this is good for now
clean %>%
  filter(type != 'end') %>%
  group_by(fund_clean) %>%
  summarize(sum = sum(money, na.rm = TRUE))
