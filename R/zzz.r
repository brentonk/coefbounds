.onAttach <- function(...)
{
    packageStartupMessage(
        "coefbounds ",
        utils::packageVersion("coefbounds"),
        "\n",
        "To get started, run vignette(\"introduction\", package = \"coefbounds\")"
    )
}
