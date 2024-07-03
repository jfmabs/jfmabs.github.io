Relatório
================
João Felipe Marques

## Indicadores de Segurança Pública e Ideologia Política

Utilizando dados provenientes do Anuário Brasileiro de Segurança Pública
e dos estudos sobre ideologia política dos partidos brasileiros de
Bolognesi (2022), comparo as médias de indicadores criminais em
diferentes mandatos com as ideologias políticas.

## Hipótese testável

### Teste a hipótese enunciada na lista 4, usando, preferencialmente, algum dos modelos que vimos em sala.

- H1: Estados governados por políticos de direita apresentam maiores
  taxas de encarceramento.

``` r
anova <- aov(quantidade_populacao_sistema_penitenciario ~ categoria_bolognesi, data = taxas_crimes)
summary(anova)
```

    ##                      Df  Sum Sq Mean Sq F value   Pr(>F)    
    ## categoria_bolognesi   3 1056979  352326   13.26 6.37e-08 ***
    ## Residuals           197 5233207   26565                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 96 observations deleted due to missingness

### Também é necessário verificar quais as diferenças de cada categoria, utilizando o teste Tukey

``` r
tukey <- TukeyHSD(anova)
print(tukey)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = quantidade_populacao_sistema_penitenciario ~ categoria_bolognesi, data = taxas_crimes)
    ## 
    ## $categoria_bolognesi
    ##                                       diff        lwr        upr     p adj
    ## Centro-Esquerda-Esquerda         118.01764   15.23785 220.797425 0.0172170
    ## Direita-Esquerda                 171.55296   99.94068 243.165249 0.0000000
    ## Extrema-Direita-Esquerda          71.22657  -41.98341 184.436561 0.3640707
    ## Direita-Centro-Esquerda           53.53533  -40.44589 147.516543 0.4539590
    ## Extrema-Direita-Centro-Esquerda  -46.79106 -175.32374  81.741621 0.7815473
    ## Extrema-Direita-Direita         -100.32639 -205.61307   4.960291 0.0680177

## Explicação do teste

### Explique, de forma sucinta, a escolha do teste, suas potencialidades e limitações.

A ANOVA foi escolhida porque a hipótese proposta compara as médias de
uma variável numérica (taxa de encarceramento dos estados) entre
diferentes categorias de uma variável categórica (ideologia política dos
governadores).

Contudo, a ANOVA por si só não é suficiente para rejeitar a hipótese
nula. É necessário verificar se há uma diferença significativa entre as
categorias da variável “categoria_bolognesi”. Para isso, foi aplicado o
teste de Tukey. Os resultados mostraram diferenças significativas entre
os grupos Centro-Esquerda e Esquerda, e entre Direita e Esquerda.

Ainda assim, a distinção entre extrema-esquerda, esquerda e
centro-esquerda (o mesmo ocorrendo com a direita) pode ter reduzido a
capacidade explicativa do modelo. Agrupar as categorias de direita e
esquerda em apenas duas variáveis poderia apresentar resultados mais
robustos.
