# -*- coding: utf-8 -*-
"""
@author: Lindsey Hornberger 

third try 
BEST
"""

from tensorflow.keras import layers, Model
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.applications import VGG16
from tensorflow.keras.models import load_model
from tensorflow.keras.preprocessing.image import load_img
from tensorflow.keras.preprocessing.image import img_to_array
from matplotlib import pyplot
import matplotlib.pyplot as plt
from tensorflow.keras.callbacks import EarlyStopping


datagen = ImageDataGenerator(
    rescale=1./255,
    validation_split=0.2,
    rotation_range=20,
    width_shift_range=0.2,
    height_shift_range=0.2,
    shear_range=0.1,
    zoom_range=0.2,
    horizontal_flip=True,
    fill_mode='nearest'
)

train_generator = datagen.flow_from_directory("/Users/lindseyhornberger/Desktop/STAT 882/CNN/FinalImages/train", 
                                              target_size=(32,32), 
                                              batch_size=32, 
                                              class_mode='binary',
                                              classes=('horses', 'deer'), 
                                              subset= 'training',
                                              shuffle=True)

validation_generator = datagen.flow_from_directory("/Users/lindseyhornberger/Desktop/STAT 882/CNN/FinalImages/train", 
                                              target_size=(32,32), 
                                              batch_size=32, 
                                              class_mode='binary',
                                              classes=('horses', 'deer'), 
                                              subset="validation",
                                              shuffle=False)



def nn_mod_binary():
    i1 = layers.Input(shape=(32, 32, 3))  # RGB images

    # First block
    o1 = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(i1)
    o2 = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(o1)
    o3 = layers.Add()([o1, o2])

    # Second block
    o4 = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(o3)
    o5 = layers.Conv2D(8, (3, 3), activation='relu', padding='same')(o4)
    o6 = layers.Add()([o4, o5])
    o6 = layers.MaxPooling2D((2, 2))(o6)

    # Third block
    o7 = layers.Conv2D(16, (3, 3), activation='relu', padding='same')(o6)
    o8 = layers.Conv2D(16, (3, 3), activation='relu', padding='same')(o7)
    o9 = layers.Add()([o7, o8])
    o9 = layers.MaxPooling2D((2, 2))(o9)

    # Dense layers
    o9 = layers.Flatten()(o9)
    o9 = layers.Dense(128, activation='relu')(o9)
    o9 = layers.Dropout(0.3)(o9)
    o9 = layers.Dense(32, activation='relu')(o9)
    o9 = layers.Dropout(0.3)(o9)
    o9 = layers.Dense(1, activation='sigmoid')(o9)

    mod = Model(inputs=i1, outputs=o9)

    mod.compile(loss='binary_crossentropy',
                optimizer='adam',
                metrics=['accuracy'])

    return mod

# Define early stopping
early_stop = EarlyStopping(
    monitor='val_loss',       # Monitor validation loss
    patience=3,               # Stop after 3 epochs with no improvement
    restore_best_weights=True  # Restore model weights from the best epoch
)

model = nn_mod_binary()
history = model.fit(
    train_generator,
    validation_data=validation_generator,
    epochs=25,
    callbacks=[early_stop]  # Add callback here
)

# Evaluate model
loss, val_accuracy = model.evaluate(validation_generator)
final_train_accuracy = history.history['accuracy'][-1]

print(f"Training accuracy: {final_train_accuracy:.4f}")
print(f"Validation accuracy: {val_accuracy:.4f}")

# Plot training and validation accuracy
plt.plot(history.history['accuracy'], label='accuracy')
plt.plot(history.history['val_accuracy'], label='val_accuracy')
plt.xlabel('Epoch')
plt.ylabel('Accuracy')
plt.ylim([0, 1])
plt.legend(loc='lower right')
plt.show()


