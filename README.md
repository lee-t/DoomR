# Algorithms used:
- Ray Casting <br />
- (Will be using BSP instead of ray casting for future expansions)
## Screenshots of graphs used to assist in building:


**Map of E1M1 plotted using ggplot**<br />
![image](https://github.com/user-attachments/assets/93905814-23bd-43fe-9559-a99890f74d51)
<br />
![image](https://github.com/user-attachments/assets/570cc7f9-8c02-4412-b9cf-c8287cf3a781)

Update (June 6th, 2026):
Reworked the renderer from basic ray-cast wall slices toward a DOOM-style sector/BSP renderer, including sector height parsing and connected wall projection for E1M1.
<img width="960" height="720" alt="image" src="https://github.com/user-attachments/assets/fd911200-1bff-4649-a33a-c075f1db652d" />

"it looks like sephora"
- Jesse

---

# DoomR R Package

This repository has been structured as an R package ready to build, check, and submit to CRAN.

## Installation

You can install the package directly from GitHub:

```R
# install.packages("remotes")
remotes::install_github("lee-t/DoomR")
```

## Usage

```R
library(DoomR)

# Render the default E1M1 map using DOOM.WAD in your current directory:
res <- doom_render(wad_path = "DOOM.WAD", verbose = TRUE)

# View the 3D projected wall rendering
print(res$render_plot)

# View the 2D top-down map of walls
print(res$map_plot)

# View the 2D top-down map of ray cast projection
print(res$ray_plot)
```

