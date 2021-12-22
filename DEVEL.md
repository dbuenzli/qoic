# Downloading test images

To download the specification test images to the [`images`](images/) directory
issue: 

```
b0 cmd download-spec-images 
```

# Test image round tripping

To round trip the images 

```sh
b0 -a trip --              # Round trip images from images/
b0 -a trip -- [FILE.qoi]…  # Round trip given images.
```



