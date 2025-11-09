# Create a Translator Object for Factor Encoding

Creates a translator object that stores the levels of specified factor
columns from a data frame. This object can be used to encode and decode
factors to integers.

Converts factor columns in a data frame to 0-based integers using the
levels stored in a translator object.

Converts integer-encoded columns back to factors using the levels stored
in a translator object.

Parses a text file and replaces encoded factor labels (e.g., `type0`,
`geno1`) with their original levels using a translator object.

## Usage

``` r
create_translator(df, cols)

encode(translator, df)

decode(translator, df_encoded)

decode_file_with_translator(output_path, translator)
```

## Arguments

- df:

  A data frame containing the same columns used to create the
  translator.

- cols:

  A character vector of column names in `df` to be translated. These
  columns must be factors.

- translator:

  A translator object created with `create_translator`, where the names
  correspond to the encoded labels to be decoded (e.g., `"type"`,
  `"geno"`).

- df_encoded:

  A data frame containing integer-encoded columns.

- output_path:

  Path to the text file to decode. The file is modified in-place.

## Value

A named list of levels for each specified column, with class
`"translator"`.

A copy of the input data frame with specified factor columns encoded as
integers.

A copy of the input data frame with specified columns converted back to
factors.

The new output_path

## Examples

``` r
df <- data.frame(
  type = factor(c("A", "B", "A")),
  geno = factor(c("X", "Y", "X"))
)
translator <- create_translator(df, c("type", "geno"))
#> Error in create_translator(df, c("type", "geno")): could not find function "create_translator"
```
