import cv2
import os
import re


os.getcwd()  # Ensure the script runs in the correct directory


# --- Configuration ---
image_folder = 'modelling/part_2/animation_frames'
video_name = 'spatial_effect_animation.mp4'
fps = 8  # Frames per second

# --- Main Script ---

# Function to extract the number from the filename for proper sorting
def get_frame_number(filename):
    match = re.search(r'(\d+)', filename)
    return int(match.group(1)) if match else -1

print("Finding image files...")
# Get all image files from the folder and sort them numerically
images = [img for img in os.listdir(image_folder) if img.endswith(".png")]
images.sort(key=get_frame_number)

if not images:
    print(f"Error: No .png images found in the '{image_folder}' directory. Please check the path.")
else:
    # Read the first image to get the frame dimensions (width, height)
    frame_path = os.path.join(image_folder, images[0])
    frame = cv2.imread(frame_path)
    height, width, layers = frame.shape
    print(f"Detected frame size: {width}x{height}")

    # Define the codec and create the VideoWriter object
    fourcc = cv2.VideoWriter_fourcc(*'mp4v') # Codec for .mp4 file
    video = cv2.VideoWriter(video_name, fourcc, fps, (width, height))

    print(f"Creating video '{video_name}' at {fps} FPS...")
    # Loop through all the image files and write them to the video
    for image in images:
        img_path = os.path.join(image_folder, image)
        video.write(cv2.imread(img_path))

    # Release the video writer object
    video.release()
    print("--- Video creation complete! --- 🎬")