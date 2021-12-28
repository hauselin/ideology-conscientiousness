# README

- `data_wide.csv`: data (variables in original scale; not centered/not z-scored)
- `data_wide_centered.csv`: (continuous variables z-scored) 

If the markdown table doesn't render well on OSF, view this file on GitHub or visit the GitHub repository to see the formatted version: https://github.com/hauselin/ideology-conscientiousness

## Columns

| Column                  | Type       | Description                                                             |
| ----------------------- | ---------- | ----------------------------------------------------------------------- |
| responseid              | character  | qualtrics userid                                                        |
| startdate               | character  | survey start date                                                       |
| attention_score         | numeric    | proportion attention check items answered correctly (4 items in total)  |
| attention_pass          | numeric    | whether attention_score == 1 (all correct)                              |
| headline_id             | character  | news sharing task headline id                                           |
| share_text              | character  | sharing decision                                                        |
| share                   | integer    | recoded share_text (main DV)                                            |
| veracity                | integer    | headline_id veracity (0 false, 1 true)                                  |
| lean_liberal            | integer    | liberal leaning headline                                                |
| lean_conservative       | integer    | conservative leaning headline                                           |
| bfi_a                   | numeric    | bigfive agreeable                                                       |
| bfi_c                   | numeric    | bigfive conscientiousness                                               |
| bfi_e                   | numeric    | bigfive extravert                                                       |
| bfi_n                   | numeric    | bigfive neurotic                                                        |
| bfi_o                   | numeric    | bigfive open                                                            |
| ctsq_aot                | numeric    | comprehensive thinking style (ctsq) - active open mind                  |
| ctsq_cmt                | numeric    | ctsq close mind                                                         |
| ctsq_pet                | numeric    | ctsq preference effortful thinking                                      |
| ctsq_pit                | numeric    | ctsq preference intuitive thinking                                      |
| gender                  | integer    | gender                                                                  |
| trans                   | integer    | gender - trans or not                                                   |
| age                     | numeric    |                                                                         |
| race                    | character  |                                                                         |
| edu                     | numeric    | education                                                               |
| income                  | integer    |                                                                         |
| demrep_c                | integer    | democrat (smaller values) or republican (larger values)                 |
| god                     | numeric    | belief in god                                                           |
| social_conserv          | numeric    | social conservatism                                                     |
| economic_conserv        | numeric    | economic conservatism                                                   |
| risk                    | numeric    | risk preference                                                         |
| trust                   | numeric    | trust other people                                                      |
| warm_repub              | numeric    | republican feeling thermometer                                          |
| warm_democrat           | numeric    | democrat feeling thermometer                                            |
| party                   | integer    | political position/party                                                |
| potus2016               | character  | 2016 election vote candidate                                            |
| potus2020               | character  | 2020 election vote candidate                                            |
| IP_Hub_recommend_block  | character  | block/remove IP or not                                                  |

