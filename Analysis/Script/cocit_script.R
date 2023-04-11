set.seed(1)
(
  cocit_fig <- final_cocit %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(clustered_co_citation %>% 
              pluck(membership) %>% 
              magrittr::set_class("numeric") %>% 
              data.frame %>% 
              rename("group" = ".") %>% 
              rownames_to_column("name"), by="name") %>% 
  mutate(group = factor(group)) %>% 
  ggraph::ggraph(., layout = "kk") +
  geom_edge_arc(aes(alpha = value),
                strength = .2,
                color = "#3A3B3C") +
  geom_node_point(aes(size = Frequency,
                      colour = group),
                  shape = 19) +
  scale_colour_manual(
    values = secondary_colors
  ) +
  scale_edge_alpha_continuous("Connection\nstrength (Weight)", range = c(0.1, .8)) +
  scale_size_continuous(breaks =c(100,300,600), range = c(1, 15)) +
  labs(size = "Number of citations") +
  theme(
    line = element_blank(),
    plot.title = element_markdown(size = 32),
    plot.subtitle = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
  panel.background = element_rect(fill = NA),
  plot.background = element_rect(fill = NA, color = NA),
  legend.position = "bottom",
  #legend.background = element_rect(fill = "#152238"),
  legend.text = element_text(colour = "black", family = "Chivo", size=14),
  legend.title = element_text(colour = "black", family="Chivo", size=14),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.box.just = "left",
  legend.title.align = 0,
  legend.justification = "left",
  legend.box = "horizontal",
  legend.direction = "horizontal"
  ) +
  guides(
    colour = "none",
    edge_alpha = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 1
    ),
    size = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 2,
      override.aes = list(colour = main_colors)
    )) 
) 

text_data <- cocit_fig$data %>% 
  arrange(desc(Frequency)) %>% 
  group_by(group) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(group = as.factor(group))

text_vandermeer <- text_data %>%
  select(x,y,group) %>% 
  
  add_row(x=c(0.3,0,0),y=c(-2,-2.3,-2.5),group=factor(rep(1,3))) %>% #Levin
  add_row(x=c(-1.9,-2.5),y=c(.5,1.2),group=factor(rep(2,2))) %>% #May
  add_row(x=c(0, -1.5, -2),y=c(2.5,3.3,3.5),group=factor(rep(3,3))) %>%  #Hubbel
  add_row(x=c(-6,-6),y=c(-2.4,-1.2),group=factor(rep(4,2))) %>% #Ulanowicz
  add_row(x=c(2.5,2.6,2.7),y=c(3,3.3,3.3),group=factor(rep(5,3))) %>%  #Ulanowicz
  arrange(group)


cocit_fig_final<-
  cocit_fig +
  geom_bspline(
    data = text_vandermeer,
    aes(x, y, group = group, colour = group),
    inherit.aes = FALSE,
    size = 1
  ) +
  geom_node_point(
    data = ~ .x %>%  group_by(group) %>% arrange(desc(Frequency)) %>%  slice(1),
    aes(x, y, size = Frequency, colour = group),
    shape = 19
  ) +
  geom_textbox(
    x = -5,
    y = -.5,
    label = "Vandermeer J, Perfecto I, Philpott  S,<br>
    <i>Ecological complexity and pest control in organic coffee production: uncovering an autonomous ecosystem service</i>,<br>
    <b>Bioscience</b>. 60, 527–537 (2010).",
    hjust = 1,
    box.colour = secondary_colors[4],
    text.color = "black",
    fill =  secondary_colors[4],
    family="Chivo"
  ) +
  geom_textbox(
    x = 2.8,
    y = 2.5,
    label = "Ulanowicz R,<br>
    <i>A phenomenological perspective of ecological development</i>,<br>
    <b>ASTM Spec Tech Publ</b>, 73–81 (1986)",
    hjust = 0.1,
    vjust = 0,
    box.colour = secondary_colors[5],
    text.color = "black",
    fill =  secondary_colors[5],
    family="Chivo"
  ) +
  geom_textbox(
    x = 0,
    y = -3.2,
    label = "Levin SA,<br>
    <i>The problem of pattern and scale in ecology</i>,<br>
    <b>Ecology</b><br> 73, 1943–1967 (1992)",
    hjust = .5,
    vjust = 0,
    box.colour = secondary_colors[1],
    text.color = "black",
    fill =  secondary_colors[1],
    family="Chivo"
  ) +
  geom_textbox(
    x = -2.5,
    y = 1,
    label = "May RM,<br>
    <i>Stability and complexity in model ecosystems</i>,<br>
    <b>Princet Univ Press</b><br>7, 1415–1419 (1973)",
    hjust = 1,
    vjust = 0,
    box.colour = secondary_colors[2],
    text.color = "black",
    fill =  secondary_colors[2],
    family="Chivo"
  ) +
  geom_textbox(
    x = -2,
    y = 2.7,
    label = "Hubbell SP,<br>
               <i>The Unified Neutral Theory of Biodiversity and Biogeography</i>,<br>
               <b>Princet Univ Press</b>, vol. 31 (2001).",
    hjust = 1,
    vjust = 0,
    box.colour = secondary_colors[3],
    text.color = "black",
    fill =  secondary_colors[3],
    family="Chivo"
  ) +
  coord_cartesian(clip = "off")
  
  
ggsave(filename = "cocit_plot.pdf", plot = cocit_fig_final, device=cairo_pdf, width= 15, height=9)

plot(rep(1,5)~1, pch=16, cex=5, col=secondary_colors)
