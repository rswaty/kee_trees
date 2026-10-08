<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.28" styleCategories="Symbology">
  <pipe>
    <rasterrenderer type="singlebandpseudocolor" band="1" opacity="1" alphaBand="-1" classificationMin="-60" classificationMax="40">
      <rastershader>
        <colorrampshader colorRampType="INTERPOLATED" classificationMode="1" clip="0" minimumValue="-60" maximumValue="40">
          <item value="-60" color="#8c510a" alpha="255" label="-60 (canopy lost)"/>
          <item value="0" color="#f6f6f6" alpha="255" label="0"/>
          <item value="40" color="#01665e" alpha="255" label="+40 (canopy gained)"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
  </pipe>
</qgis>
