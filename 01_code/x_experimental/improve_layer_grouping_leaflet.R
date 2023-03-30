# chat GPT idea
library(leaflet)

# create map object
m <- leaflet()

# add layers with group assignments
layer1 <- addMarkers(m, data = some_data1, group = c("A", "B"))
layer2 <- addMarkers(m, data = some_data2, group = c("B", "C"))
layer3 <- addMarkers(m, data = some_data3, group = c("A", "C"))

# create layers control with overlayGroups set to A, B, C
m %>% addLayersControl(
  overlayGroups = c("A", "B", "C"),
  options = layersControlOptions(collapsed = FALSE)
) %>%
  # add custom JavaScript to hide/show layers based on selected groups
  onRender("
    function() {
      // create an object to store the layer-group relationships
      var layerGroups = {
        'layer1': ['A', 'B'],
        'layer2': ['B', 'C'],
        'layer3': ['A', 'C']
      };

      // function to determine if a layer should be displayed based on selected groups
      function shouldDisplayLayer() {
        // get the currently selected groups
        var selectedGroups = [];
        $('.leaflet-control-layers-selector:checked').each(function() {
          selectedGroups.push($(this).attr('name'));
        });

        // loop through each layer and check if it should be displayed
        $('.leaflet-marker-icon').each(function() {
          var layerName = $(this).closest('.leaflet-marker-pane').attr('id');
          var layerGroupsList = layerGroups[layerName];

          if (layerGroupsList.every(group => selectedGroups.includes(group))) {
            $(this).show();
          } else {
            $(this).hide();
          }
        });
      }

      // call shouldDisplayLayer when the layers control is updated
      $('.leaflet-control-layers-selector').on('change', function() {
        shouldDisplayLayer();
      });
    }
  ")


