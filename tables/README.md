# Descriptive Tables

## Scripts

-   **all_tables.qmd** = streamlined table production file with comments
-   **table_utils.R** = utility functions for creating standard tables and formatting them (APA)
-   **conv_stats.R** = script to recreate conversation stats, and merge old iterations
    -   -\> "added_vars_final_UPDATE.rds" create where?

        -   from **Cleaning/more_variables.R**

        -   added in **subsets/create_split_data.R**
-   **subsets/** = scripts to create the necessary data subsets for each subtable

## Datasets

-   **all_split.rds** = list with all top-level categories as elements

    ```{r}
    split_all_dat <- list(
      dataset = dat,
      twlz = all_TWLZ,
      chats = all_chats,
      subjects = all_subjects,
      states = all_states
    )
    ```

-   

-   
