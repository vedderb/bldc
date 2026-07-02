/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef QLBMQUICKDISPLAYITEM_H_
#define QLBMQUICKDISPLAYITEM_H_

#include <QQuickItem>
#include <QImage>
#include <QMutex>

class QLbmQuickDisplayItem : public QQuickItem {
  Q_OBJECT
public:
  explicit QLbmQuickDisplayItem(int displayWidth, int displayHeight,
                                QQuickItem *parent = nullptr);

public slots:
  void setImage(const QImage &img);
  void setImageAt(int x, int y, const QImage &img);

protected:
  QSGNode *updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *) override;

private:
  QMutex m_mutex;
  QImage m_image;
  bool   m_pending;
  int    m_displayWidth;
  int    m_displayHeight;
};

#endif
