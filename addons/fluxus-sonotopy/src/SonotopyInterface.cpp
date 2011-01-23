// Copyright (C) 2011 Alexander Berman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "SonotopyInterface.h"

using namespace sonotopy;

SonotopyInterface::SonotopyInterface(int sampleRate, int bufferSize) {
  audioParameters.sampleRate = sampleRate;
  audioParameters.bufferSize = bufferSize;

  spectrumAnalyzer = new SpectrumAnalyzer();
  spectrumBinDivider = new SpectrumBinDivider(sampleRate,
                                              spectrumAnalyzer->getSpectrumResolution());
  vane = new Vane(audioParameters);
  beatTracker = new BeatTracker(spectrumBinDivider->getNumBins(), bufferSize, sampleRate);

  gridMapCircuit = new GridMapCircuit(audioParameters, gridMapCircuitParameters);
  gridMapWidth = gridMapCircuitParameters.gridWidth;
  gridMapHeight = gridMapCircuitParameters.gridHeight;

  waveformCircularBuffer = NULL;
  waveformBuffer = NULL;
  numWaveformFrames = 0;
}

SonotopyInterface::~SonotopyInterface() {
  delete vane;
  delete beatTracker;
  delete spectrumBinDivider;
  delete spectrumAnalyzer;
  delete gridMapCircuit;
  if(waveformCircularBuffer != NULL)
    delete waveformCircularBuffer;
  if(waveformBuffer != NULL)
    delete [] waveformBuffer;
}

void SonotopyInterface::feedAudio(const float *buffer, unsigned long numFrames) {
  spectrumAnalyzer->feedAudioFrames(buffer, numFrames);
  spectrumBinDivider->feedSpectrum(spectrumAnalyzer->getSpectrum(), numFrames);
  beatTracker->feedFeatureVector(spectrumBinDivider->getBinValues());
  vane->feedAudio(buffer, numFrames);
  gridMapCircuit->feedAudio(buffer, numFrames);
  if(waveformCircularBuffer != NULL) {
    waveformCircularBuffer->write(numFrames, buffer);
    waveformCircularBuffer->moveReadHead(numFrames);
  }
}

float SonotopyInterface::getVaneAngle() {
  return vane->getAngle();
}

float SonotopyInterface::getBeatIntensity() {
  return beatTracker->getIntensity();
}

int SonotopyInterface::getNumSpectrumBins() {
  return spectrumBinDivider->getNumBins();
}

float SonotopyInterface::getSpectrumBinValue(int bin) {
  if(bin < 0 || bin >= (int) spectrumBinDivider->getNumBins())
    return 0.0f;
  else {
    const float *binValues = spectrumBinDivider->getBinValues();
    return normalizer.normalize(binValues[bin]);
  }
}

void SonotopyInterface::setWaveformWindowSize(float secs) {
  int newNumWaveformFrames = (int) (audioParameters.sampleRate * secs);
  if(waveformCircularBuffer != NULL) {
    if(newNumWaveformFrames != numWaveformFrames) {
      delete waveformCircularBuffer;
      waveformCircularBuffer = NULL;
      delete [] waveformBuffer;
      waveformBuffer = NULL;
      numWaveformFrames = 0;
    }
  }

  if(waveformCircularBuffer == NULL) {
    numWaveformFrames = newNumWaveformFrames;
    waveformCircularBuffer = new CircularBuffer<float>(numWaveformFrames);
    waveformBuffer = new float [numWaveformFrames];
  }
}

int SonotopyInterface::getNumWaveformFrames() {
  return numWaveformFrames;
}

const float *SonotopyInterface::getWaveformBuffer() {
  if(waveformCircularBuffer != NULL)
    waveformCircularBuffer->read(numWaveformFrames, waveformBuffer);
  return waveformBuffer;
}

unsigned int SonotopyInterface::getGridMapWidth() {
  return gridMapWidth;
}

unsigned int SonotopyInterface::getGridMapHeight() {
  return gridMapHeight;
}

const SonogramMap::ActivationPattern* SonotopyInterface::getGridMapActivationPattern() {
  return gridMapCircuit->getActivationPattern();
}

float SonotopyInterface::getGridMapActivation(unsigned int x, unsigned int y) {
  if(x >= gridMapWidth) return 0.0f;
  if(y >= gridMapHeight) return 0.0f;
  return gridMapCircuit->getActivation(x, y);
}
