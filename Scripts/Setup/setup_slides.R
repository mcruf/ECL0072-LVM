#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                               #
#      Slide customization      #
#                               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(xaringanthemer)


# Define main slide colors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inverse_slide <- "#dedbd2" #e3d5ca, dedbd2
metropolis_dark  <- "#242423" 
metropolis_accent <- "#d5bdaf" #d4a373, d5bdaf, 778da9, 415a77



style_xaringan(
  # Base settings
  background_color = "#FAFAFA",
  text_color       = metropolis_dark,
  header_color     = "#846267", #6b705c
  
  # Typography matching Metropolis
  header_font_google = google_font("Josefin Sans", "400"),
  text_font_google   = google_font("Josefin Sans", "350", "350i", "300"),
  code_font_google   = google_font("Fira Mono"),
  
  ## Header font size
  header_h1_font_size = "2.5rem",
  header_h2_font_size = "1.75rem",
  header_h3_font_size = "1.5rem",
  
  ## Text font size
  #text_font_size = "95%", # e.g., "18px", "90%", or "1.1rem"
  
  # Inject explicit CSS rules for the top Metropolis header bar
  extra_css = list(
    
    ### Reduce code chunk font size
    ".remark-code" = list(
      "font-size" = "80%" # Adjust percentage as needed (e.g., 70%, 80%, 14px)
    ),
    ".remark-code-line" = list(
      "font-size" = "80%"
    ),
    
    
    # Text with different sizes 
    ".small-text" = list("font-size" = "80%"),
    ".small-text2" = list("font-size" = "90%"),
    ".small-text3" = list("font-size" = "70%"),
    
    # Decrease vertical spacing in listed items
    "li > p" = list(
      "margin-top"    = "0px !important",
      "margin-bottom" = "0px !important"),
    
    "ul li, ol li" = list(
      "margin-bottom" = "4px !important",
      "line-height"   = "1.3 !important"),
    
    "ul, ol" = list(
      "margin-top"  = "0.2em !important",
      "padding-top" = "0px !important"),
    
    "p" = list(
      "margin-bottom" = "0.3em !important"),
    
    
    # Adjust footnote position to reduce margins from slide borders
    ".footnote" = list(
      "position"      = "absolute",
      "bottom"        = "20px",  # Pushes footnote closer to the bottom edge (default is ~3em)
      "left"          = "50px",  # Moves footnote closer to the left edge
      "font-size"     = "70%",   # Optional: keeps text size small
      "line-height"   = "1.2"    # Optional: compact line spacing
    ),
    
    ### Create gray box
    ".gray-box" = list("background-color" = "#e9ecef",
                       "border-radius"    = "25px",
                       "padding"          = "10px 25px",
                       "display"          = "inline-block", # Shrinks container width to fit content
                       "margin"           = "5px 0px"),
    
    
    ".gray-box-big" = list("background-color" = "#e9ecef",
                           "border-radius"    = "10px",
                           "padding"          = "10px 100px", # Increased slightly for a clean look
                           "width"            = "fit-content",
                           "margin"           = "0px auto"),
    
    
    ".gray-box-small" = list("background-color" = "#e9ecef",
                             "border-radius"    = "10px",
                             "padding"          = "10px 50px", # Increased slightly for a clean look
                             "width"            = "fit-content",
                             "margin"           = "0px auto"),
    
    ### Salmon box
    ".salmon-box" = list("background-color" = "#e07a5f",
                         "border-radius"    = "10px",
                         "display"          = "inline-block", # Shrinks container width to fit content
                         "padding"          = "5px 50px",
                         "margin"           = "0px 50px"),
    
    
    ### Three column display
    ".pull-three-left" = list(
      "float" = "left",
      "width" = "31%"
    ),
    ".pull-three-center" = list(
      "float"       = "left",
      "width"       = "31%",
      "margin-left" = "3.5%"
    ),
    ".pull-three-right" = list(
      "float" = "right",
      "width" = "31%"
    ),
    
    
    
    # Create the horizontal top banner box on every standard slide
    
    # Global slide padding (REDUCED MARGINS HERE)
    ".remark-slide-content" = list(
      "padding-top"      = "40px",  # Keeps content clear of the top banner
      "padding-bottom"   = "20px",  # Reduced bottom margin
      "padding-left"     = "35px",  # Reduced left margin
      "padding-right"    = "35px",  # Reduced right margin
      "background-image" = paste0("linear-gradient(to bottom, ", metropolis_accent, " 40px, transparent 40px)")
    ),
    
    # Ensure titles render in white text sitting perfectly on top of the banner
    ".remark-slide-content h1" = list(
      "position" = "absolute",
      "top" = "15px",
      "left" = "750px",
      "color" = "#FFFFFF",
      "margin" = "0",
      "font-size" = "10px"
    ),
    
    # Styled block for inverse/dark divider slides
    ".inverse" = list(
      "background-color" = inverse_slide,
      "background-image" = "none", # Overwrite top bar style for dark slides
      "color" = "#FFFFFF"
    ),
    
    
    ### ==========================================
    ### Estilos para a Capa Personalizada (Title Slide)
    ### ==========================================
    ".title-slide" = list(
      "background-color" = "#FFFFFF !important", # FORÇA O FUNDO BRANCO
      "background-image" = "url('../Figuras/Logo_course.png') !important", # Adicionado ../ se necessário
      "background-position" = "left bottom !important",
      "background-size" = "70% auto !important", # Tamanho ajustado da imagem geométrica
      "padding" = "40px 60px 40px 60px !important"
    ),
    
    # Bloco do Título Principal e Data (Topo)
    ".title-block" = list(
      "border-bottom" = "2px solid #333333",
      "padding-bottom" = "8px",
      "margin-top" = "100px",
      "margin-bottom" = "20px",
      "display" = "flex",
      "justify-content" = "space-between",
      "align-items" = "flex-end"
    ),
    ".title-block h1" = list(
      "font-family" = "'Josefin Sans', sans-serif !important",
      "font-size" = "1.5rem !important",  # REDUZIDO: ajuste se quiser maior/menor
      "font-weight" = "700 !important",
      "color" = "#111111 !important",
      "margin" = "0 !important",
      "line-height" = "1.2 !important",
      "position" = "static !important", # Cancela top/left absoluto dos outros slides
      "text-align" = "left",
      "max-width" = "75%"
    ),
    ".title-block .date-text" = list(
      "font-size" = "1.0rem !important",  # REDUZIDO
      "color" = "#555555 !important",
      "font-weight" = "400"
    ),
    
    # Bloco do Subtítulo / Aula (Meio - Direita)
    ".subtitle-block" = list(
      "text-align" = "right",
      "margin-top" = "20px",
      "margin-bottom" = "40px"
    ),
    ".subtitle-block h2" = list(
      "font-family" = "'Josefin Sans', sans-serif !important",
      "font-size" = "1.3rem !important",  # REDUZIDO
      "font-weight" = "600 !important",
      "color" = "#111111 !important",
      "margin" = "0 0 10px 0 !important",
      "text-transform" = "uppercase"
    ),
    ".subtitle-block .author-name" = list(
      "font-size" = "1.0rem !important",  # REDUZIDO
      "color" = "#333333 !important",
      "margin" = "3px 0"
    ),
    ".subtitle-block .author-email" = list(
      "font-size" = "0.9rem !important",  # REDUZIDO
      "color" = "#846267 !important",
      "margin" = "0"
    ),
    
    # Logo no canto inferior direito
    ".cover-logo" = list(
      "position" = "absolute !important",
      "bottom" = "30px !important",
      "right" = "50px !important",
      "height" = "35px !important", # Ajuste a altura
      "width" = "auto !important"
    )
    
  )
)
